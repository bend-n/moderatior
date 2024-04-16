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
                Nick {
                    old: Some(old),
                    new: Some(new),
                } => format!("nick ~~{old}~~ {RIGHT} {new}"),
                Nick { new: Some(new), .. } => format!("nick {ADD} {new}"),
                Name {
                    old: Some(old),
                    new: Some(new),
                } => format!("name ~~{old}~~ {RIGHT} {new}"),
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
                Color {
                    old: Some(old),
                    new: Some(new),
                } => match (old, new) {
                    (0, new) => format!("color {RIGHT} #{new:x}"),
                    (old, 0) => format!("color ~~#{old:x}~~ {RIGHT} no color"),
                    (old, new) => format!("color ~~#{old:x}~~ {RIGHT} #{new:x}"),
                },
                Topic {
                    old: Some(old),
                    new: Some(new),
                } => format!("topic ```diff{}\n```", diff(old, new)),
                Topic {
                    old: None,
                    new: Some(new),
                } => format!("topic {new}"),
                RateLimitPerUser {
                    old: Some(old),
                    new: Some(new),
                } => {
                    let old_f = humantime::format_duration(Duration::from_secs(*old as _));
                    let new_f = humantime::format_duration(Duration::from_secs(*new as _));
                    match (old, new) {
                        (_, 0) => format!("slowmode {CANCEL} (from {old_f})",),
                        (0, _) => format!("slowmode {RIGHT} {new_f}"),
                        (_, _) => format!("slowmode ~~{old_f}~~ {RIGHT} {new_f}"),
                    }
                }
                Permissions {
                    old: Some(old),
                    new: Some(new),
                } => {
                    let list = |&x: &serenity::all::Permissions| {
                        x.iter_names()
                            .map(|(x, _)| x.replace("_", " ").to_lowercase())
                            .reduce(|a, b| format!("{a}\n{b}"))
                            .unwrap_or_else(String::new)
                    };
                    let new = list(new);
                    if old.bits() == 0 {
                        return Some(format!("permissions {RIGHT} {new}"));
                    }
                    let old = list(old);
                    let mut removed = String::new();
                    let mut added = String::new();
                    diff::lines(&old, &new).into_iter().for_each(|diff| {
                        use std::fmt::Write;
                        match diff {
                            diff::Result::Left(l) => write!(&mut added, "{l} ").unwrap(),
                            diff::Result::Right(r) => write!(&mut removed, "{r} ").unwrap(),
                            _ => (),
                        }
                    });
                    let removed = match &*removed {
                        "" => removed,
                        x => format!("- {x}\n"),
                    };
                    let added = match &*added {
                        "" => added,
                        x => format!("+ {x}\n"),
                    };
                    format!("permissions ```diff\n{removed}{added}```")
                }
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
            Channel(a) => format!(
                "{} [channel]({})\n{changes}",
                match a {
                    ChannelAction::Create => ADD,
                    ChannelAction::Delete => CANCEL,
                    ChannelAction::Update => ROTATE,
                    _ => return None,
                },
                format!(
                    "https://discord.com/channels/925674713429184564/{}",
                    log.target_id?
                )
            ),
            Role(RoleAction::Update) => format!("{ROTATE} role <@&{}>\n{changes}", log.target_id?),
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

fn diff(old: &str, new: &str) -> String {
    diff::lines(old, new)
        .into_iter()
        .map(|diff| match diff {
            diff::Result::Left(l) => format!("- {l}"),
            diff::Result::Right(r) => format!("+ {r}"),
            diff::Result::Both(l, _) => format!("  {l}"),
        })
        .fold(String::new(), |a, b| format!("{a}\n{b}"))
        .replace('`', "\u{200b}`")
}

pub struct Bot;
impl Bot {
    pub async fn spawn() {
        println!("bot startup");
        let tok =
            std::env::var("TOKEN").unwrap_or_else(|_| read_to_string("token").expect("wher token"));
        let f = poise::Framework::builder()
            .options(poise::FrameworkOptions {
                commands: vec![help(), reload()],
                on_error: |e| Box::pin(on_error(e)),
                event_handler: |c, e, _, _| {
                    Box::pin(async move {
                        match e {
                            FullEvent::GuildAuditLogEntryCreate { entry, .. } => {
                                let Some(h) = format(entry.clone()) else {
                                    return Ok(());
                                };
                                ChannelId::new(1220060625338761286).send_message(
                                    c,
                                    CreateMessage::new()
                                        .allowed_mentions(CreateAllowedMentions::new().empty_users().empty_roles())
                                        .content(h),
                                )
                                .await?;
                            }
                            FullEvent::GuildMemberAddition { new_member } => {
                                ChannelId::new(944772532559568936)
                                    .send_message(
                                        c,
                                        CreateMessage::new()
                                            .content(format!("{RIGHT} hi <@{}>", new_member.user.id),
                                    ))
                                    .await
                                    .unwrap();                                
                            }
                            FullEvent::GuildMemberRemoval { user, member_data_if_available, .. } => {
                                ChannelId::new(944772532559568936)
                                    .send_message(
                                        c,
                                        CreateMessage::new()
                                            .content(format!("{LEFT} goodbye {} <@{}>", match member_data_if_available {
                                                Some(x) => x.nick.as_ref().unwrap_or(&user.name),
                                                None => &user.name,
                                            }, user.id)),
                                    )
                                    .await
                                    .unwrap();
                            },
                            FullEvent::Message { new_message } => db::set_m(new_message.clone()),
                            FullEvent::MessageUpdate { event: MessageUpdateEvent { id, channel_id, content: Some(content), author: Some(User{ id: author, bot, ..}), attachments: Some(attachments), .. }, .. } if !bot => {
                                if let Some((oc, _, _)) = db::get(id.get()) {
                                    let diff = diff(&oc, content);
                                    ChannelId::new(1226396559185285280)
                                    .send_message(
                                        c,
                                        CreateMessage::new()
                                            .allowed_mentions(
                                                CreateAllowedMentions::new()
                                                    .empty_users()
                                                    .empty_roles(),
                                            )
                                            .content(format!("<@{author}> edited their message https://discord.com/channels/925674713429184564/{channel_id}/{id}\n```diff{diff}```")),
                                    )
                                    .await
                                    .unwrap();
                                }
                                db::set(id.get(), (content.clone(), attachments.iter().map(|a|a.url.clone()).collect(), author.get()))
                            },
                            FullEvent::MessageDelete {
                                deleted_message_id, channel_id, ..
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
                                                            "<@{a}> {CANCEL} deleted their own message (in <#{channel_id}>):```\n{content}\n```\n{}",
                                                            links
                                                                .into_iter()
                                                                .reduce(|a, b| format!("{a} {b}"))
                                                                .unwrap_or_else(String::new)
                                                        )
                                                    } else {
                                                        format!(
                                                            "<@{who}> {CANCEL} deleted message by <@{author}> (in <#{channel_id}>):```\n{content}\n```\n{}",
                                                            links
                                                                .into_iter()
                                                                .reduce(|a, b| format!("{a} {b}"))
                                                                .unwrap_or_else(String::new)
                                                        )
                                                    }
                                                }
                                                None => format!("<@{who}> {CANCEL} deleted message by <@{author}> in <#{channel_id}> (content unavailable)"),
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
        ClientBuilder::new(tok, GatewayIntents::all())
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
