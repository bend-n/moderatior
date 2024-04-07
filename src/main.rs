#![feature(if_let_guard, let_chains, lazy_cell)]
use anyhow::Result;
use emoji::named::*;
use poise::{serenity_prelude::*, CreateReply};
use std::convert::identity;
use std::fs::read_to_string;
use std::pin::pin;
use std::time::Duration;
mod db;
#[macro_export]
macro_rules! send {
    ($e:expr, $fmt:literal $(, $args:expr)* $(,)?) => {
        $e.send(format!($fmt $(, $args)*))
    };
}

fn last() -> u64 {
    std::fs::read("last")
        .ok()
        .map(|x| {
            x.into_iter()
                .fold(0u64, |acc, x| acc * 10 + (x - b'0') as u64)
        })
        .unwrap_or(0)
}

fn set_last(x: u64) {
    std::fs::write("last", x.to_string()).unwrap();
}

pub fn format(log: AuditLogEntry) -> Option<String> {
    use serenity::model::guild::audit_log::Action::*;
    use serenity::model::guild::audit_log::Change::*;
    let changes = log
        .changes
        .iter()
        .flatten()
        .into_iter()
        .map(|x| {
            Some(match x {
                Nick { old: Some(old), new: Some(new) } => format!("nick ~~{old}~~ {RIGHT} {new}"),
                Nick { new: Some(new),.. } => format!("nick {ADD} {new}"),
                Name { old: Some(old), new: Some(new) } => format!("name ~~{old}~~ {RIGHT} {new}"),
                Name { new: Some(new), .. } => format!("name {ADD} {new}"),
                RolesRemove { new, .. } => new
                    .as_ref()?
                    .into_iter()
                    .map(|x| format!("{CANCEL} <@&{}>", x.id))
                    .reduce(|a, b| format!("{a} {b}"))
                    .unwrap_or_else(String::new),
                RolesAdded { new, .. } => new
                    .as_ref()?
                    .into_iter()
                    .map(|x| format!("{ADD} <@&{}>", x.id))
                    .reduce(|a, b| format!("{a} {b}"))
                    .unwrap_or_else(String::new),
                _ => return None,
            })
        })
        .filter_map(identity)
        .reduce(|a, b| format!("{a}\n{b}"))
        .unwrap_or_else(String::new);
    Some(format!(
        "<t:{}:d> <@{}> {}",
        log.id.created_at().unix_timestamp(),
        log.user_id,
        match log.action {
            GuildUpdate => format!("guild changes\n{changes}"),
            Channel(ChannelAction::Create) => format!("{ADD} channel\n{changes}"),
            Channel(ChannelAction::Delete) => format!("{CANCEL} channel\n{changes}"),
            Channel(ChannelAction::Update) => format!("{ROTATE} channel\n{changes}"),
            Member(MemberAction::Kick | MemberAction::BanAdd) =>
                format!("{HAMMER} <@{}>", log.target_id?),
            Member(MemberAction::RoleUpdate)
                if let Some(t) = log.target_id
                    && t.get() != log.user_id.get() =>
            {
                format!("{ROTATE} roles of <@{t}>: {changes}")
            }
            Member(MemberAction::RoleUpdate) => format!("{ROTATE} roles: {changes}"),
            _ => return None,
        }
    ))
}

async fn lop(c: serenity::client::Context) {
    let c = &c;
    let g = c.http().get_guild(925674713429184564.into()).await.unwrap();
    let ch = g.channel_id_from_name(c, "server-logs").unwrap();
    loop {
        let l = last();
        for log in g
            .audit_logs(c, None, None, None, None)
            .await
            .unwrap()
            .entries
            .into_iter()
            .rev()
            .filter(|x| x.id.created_at().unix_timestamp() as u64 > l)
        {
            let Some(h) = format(log.clone()) else {
                continue;
            };
            ch.send_message(
                c,
                CreateMessage::new()
                    .allowed_mentions(CreateAllowedMentions::new().empty_users().empty_roles())
                    .content(h),
            )
            .await
            .unwrap();
        }
        set_last(
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs(),
        );
        // every minute
        tokio::time::sleep(Duration::from_secs(60)).await;
    }
}
pub struct Bot;
impl Bot {
    pub async fn spawn() {
        println!("bot startup");
        let tok =
            std::env::var("TOKEN").unwrap_or_else(|_| read_to_string("token").expect("wher token"));
        let f = poise::Framework::builder()
            .options(poise::FrameworkOptions {
                commands: vec![help(), prune(), reload(), run()],
                on_error: |e| Box::pin(on_error(e)),
                event_handler: |c, e, _, _| {
                    Box::pin(async move {
                        match e {
                            FullEvent::Message { new_message } => db::set_m(new_message.clone()),
                            FullEvent::MessageUpdate { event: MessageUpdateEvent { id, channel_id, content: Some(content), author: Some(User{ id: author, bot, ..}), attachments: Some(attachments), .. }, .. } if !bot => {
                                if let Some((oc, _, _)) = db::get(id.get()) {
                                    let diff = diff::lines(content, &oc).into_iter().map(|diff| {
                                        match diff {
                                            diff::Result::Left(l) => format!("+ {l}"),
                                            diff::Result::Right(r) => format!("- {r}"),
                                            diff::Result::Both(l, _) => format!("  {l}"),
                                        }
                                    }).fold(String::new(), |a,b| format!("{a}\n{b}"));
                                    ChannelId::new(1226396559185285280)
                                    .send_message(
                                        c,
                                        CreateMessage::new()
                                            .allowed_mentions(
                                                CreateAllowedMentions::new()
                                                    .empty_users()
                                                    .empty_roles(),
                                            )
                                            .content(format!("<t:{}:d> <@{author}> edited their message https://discord.com/channels/925674713429184564/{channel_id}/{id}\n```diff{diff}```", std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_secs())),
                                    )
                                    .await
                                    .unwrap();   
                                }
                                db::set(id.get(), (content.clone(), attachments.iter().map(|a|a.url.clone()).collect(), author.get()))
                            },
                            FullEvent::MessageDelete {
                                deleted_message_id, ..
                            } => {
                                let log = c.http().get_guild(925674713429184564.into()).await.unwrap()
                                    .audit_logs(c, Some(audit_log::Action::Message(MessageAction::Delete)), None, None, Some(1)).await?
                                    .entries.into_iter().next().unwrap();
                                let (author, who) = (log.target_id.unwrap(), log.user_id);
                                ChannelId::new(1226396559185285280)
                                    .send_message(
                                        c,
                                        CreateMessage::new()
                                            .allowed_mentions(
                                                CreateAllowedMentions::new()
                                                    .empty_users()
                                                    .empty_roles(),
                                            )
                                            .content(match db::get(deleted_message_id.get()) {
                                                Some((content, links, a)) => {
                                                    if a == 1224510735959462068 { return Ok(()) }
                                                    if author.get() != a {
                                                        format!(
                                                            "<t:{}:d> <@{a}> {CANCEL} deleted their own message:\n{content}\n\n{}",
                                                            std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_secs(),
                                                            links
                                                                .into_iter()
                                                                .reduce(|a, b| format!("{a} {b}"))
                                                                .unwrap_or_else(String::new)
                                                        )
                                                    } else {
                                                        format!(
                                                            "<t:{}:d> <@{who}> {CANCEL} deleted message by <@{author}>:\n{content}\n\n{}",
                                                            std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_secs(),
                                                            links
                                                                .into_iter()
                                                                .reduce(|a, b| format!("{a} {b}"))
                                                                .unwrap_or_else(String::new)
                                                        )
                                                    }
                                                }
                                                None => format!("<@{who}> {CANCEL} deleted message by <@{author}>"),
                                                
                                            }),
                                    )
                                    .await
                                    .unwrap();
                            }
                            _ => (),
                        }
                        Ok(())
                    })
                },
                ..Default::default()
            })
            .setup(|ctx, _ready, f| {
                Box::pin(async move {
                    poise::builtins::register_globally(ctx, &f.options().commands).await?;
                    println!("registered");
                    Ok(())
                })
            })
            .build();
        ClientBuilder::new(
            tok,
            GatewayIntents::non_privileged() | GatewayIntents::MESSAGE_CONTENT,
        )
        .framework(f)
        .await
        .unwrap()
        .start()
        .await
        .unwrap();
    }
}
type Context<'a> = poise::Context<'a, (), anyhow::Error>;

async fn on_error(error: poise::FrameworkError<'_, (), anyhow::Error>) {
    use poise::FrameworkError::Command;
    match error {
        Command { error, ctx, .. } => {
            ctx.say(format!("<@696196765564534825> {error}"))
                .await
                .unwrap();
        }
        err => poise::builtins::on_error(err).await.unwrap(),
    }
}

#[poise::command(slash_command)]
/// ask for information
pub async fn help(ctx: Context<'_>) -> Result<()> {
    ctx.send(
        poise::CreateReply::default()
            .ephemeral(true)
            .content(include_str!("help.md")),
    )
    .await?;
    Ok(())
}
const OWNER: u64 = 696196765564534825;
#[poise::command(slash_command)]
/// Run the logger
pub async fn run(c: Context<'_>) -> Result<()> {
    if c.author().id != OWNER {
        poise::say_reply(c, "access denied. this incident will be reported").await?;
        return Ok(());
    }
    poise::say_reply(c, OK).await?;
    tokio::spawn(lop(c.serenity_context().clone()));
    Ok(())
}

#[poise::command(slash_command)]
/// Prune own messages.
pub async fn prune(c: Context<'_>) -> Result<()> {
    if c.author().id != OWNER {
        poise::say_reply(c, "access denied. this incident will be reported").await?;
        return Ok(());
    }
    let h = poise::say_reply(c, "working...").await?;
    let hm = h.message().await.unwrap().id;
    let g = c.http().get_guild(925674713429184564.into()).await.unwrap();
    let ch = g.channel_id_from_name(c, "server-logs").unwrap();
    let mut strm = pin!(ch.messages_iter(c));
    while let Some(Ok(next)) = futures::StreamExt::next(&mut strm).await {
        if next.id == hm {
            continue;
        }
        if next.is_own(c) {
            next.delete(c).await?;
        }
    }
    h.edit(c, CreateReply::default().content(OK)).await?;
    std::fs::write("last", &[b'0']).unwrap();
    Ok(())
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    Bot::spawn().await;
}

#[poise::command(slash_command)]
pub async fn reload(c: Context<'_>) -> Result<()> {
    if c.author().id != OWNER {
        poise::say_reply(c, "access denied. this incident will be reported").await?;
        return Ok(());
    }
    let h = poise::say_reply(c, "0 complete").await?;
    let mut n = 0u64;
    let chs = c.guild().unwrap().channels.clone();
    for ch in chs.keys() {
        let mut stream = pin!(ch.messages_iter(c));
        while let Some(Ok(next)) = futures::StreamExt::next(&mut stream).await {
            if db::get(next.id.get()).is_some() {
                break;
            }
            n += 1;
            db::set_m(next);
        }
        _ = h
            .edit(
                c,
                CreateReply::default().content(format!("+{n}, {:.2} mbs", db::sz())),
            )
            .await;
    }
    h.edit(
        c,
        CreateReply::default().content(format!("+{n}: {:.2} mbs", db::sz())),
    )
    .await?;
    Ok(())
}
