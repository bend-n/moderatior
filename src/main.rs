#![feature(if_let_guard, let_chains, lazy_cell)]
#![allow(confusable_idents, mixed_script_confusables)]
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

const LEFT: &str = "<:l:1231827542676344892>";
const RIGHT: &str = "<:r:1231826176197005363>";
const ADD: &str = "<:a:1231823256672145449>";
const ROTATE: &str = "<:c:1231822613115043871>";
const CANCEL: &str = "<:x:1231825482572369980>";
const EDIT: &str = "<:edi:1231825459688378460>";

pub fn format(log: AuditLogEntry) -> Option<String> {
    use serenity::model::guild::audit_log::Action::*;
    use serenity::model::guild::audit_log::Change::*;
    macro_rules! pick {
        ($t:ident) => {
            log.changes
                .iter()
                .flatten()
                .into_iter()
                .find(|y| matches!(y, $t { .. }))
        };
    }
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
        "<@{}> {}",
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
            Thread(ThreadAction::Delete)
                if let Some(Name {
                    new: None,
                    old: Some(old),
                }) = pick!(Name) =>
                format!("{CANCEL} deleted thread `{old}`"),
            Role(RoleAction::Update) => format!("{ROTATE} role <@&{}>\n{changes}", log.target_id?),
            Member(MemberAction::RoleUpdate)
                if let Some(t) = log.target_id
                    && t.get() != log.user_id.get() =>
            {
                format!("{ROTATE} roles of <@{t}>: {changes}")
            }
            Member(MemberAction::RoleUpdate) => format!("{ROTATE} roles: {changes}"),
            Member(MemberAction::BanAdd) => format!(
                "{CANCEL} banned <@{}> (read messages with </spy:1237586279156416592>)",
                log.target_id?
            ),
            Member(MemberAction::BanRemove) => format!("{CANCEL} unbanned <@{}>", log.target_id?),
            Member(MemberAction::Kick) => format!("{CANCEL} kicked <@{}>", log.target_id?),
            Member(MemberAction::Update)
                if let Some(CommunicationDisabledUntil { new: Some(new), .. }) =
                    pick!(CommunicationDisabledUntil) =>
            {
                format!(
                    "<:mut:1239018529970327673> <@{}> until <t:{}:d> ({}): {}",
                    log.target_id?,
                    new.unix_timestamp(),
                    humantime::format_duration(
                        new.signed_duration_since(*log.id.created_at())
                            .to_std()
                            .ok()?,
                    ),
                    log.reason?
                )
            }
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

async fn event_handler(c: &serenity::all::Context, e: &FullEvent) -> Result<()> {
    match e {
        FullEvent::GuildAuditLogEntryCreate { entry, .. } => {
            let Some(h) = format(entry.clone()) else {
                return Ok(());
            };
            ChannelId::new(1220060625338761286)
                .send_message(
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
                    CreateMessage::new().content(format!("{RIGHT} hi <@{}>", new_member.user.id)),
                )
                .await
                .unwrap();
        }
        FullEvent::GuildMemberRemoval {
            user,
            member_data_if_available,
            ..
        } => {
            ChannelId::new(944772532559568936)
                .send_message(
                    c,
                    CreateMessage::new().content(format!(
                        "{LEFT} goodbye {} <@{}>",
                        match member_data_if_available {
                            Some(x) => x.nick.as_ref().unwrap_or(&user.name),
                            None => &user.name,
                        },
                        user.id
                    )),
                )
                .await
                .unwrap();
        }
        FullEvent::Message { new_message } => db::set_m(new_message.clone()),
        FullEvent::MessageUpdate {
            event:
                MessageUpdateEvent {
                    id,
                    channel_id,
                    content: Some(content),
                    author:
                        Some(User {
                            id: author, bot, ..
                        }),
                    attachments: Some(attachments),
                    ..
                },
            ..
        } if !bot => {
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
                        .content(format!(
                            "<t:{}:d> <@{author}> {EDIT} their message https://discord.com/channels/925674713429184564/{channel_id}/{id}\n```diff{diff}```",
                            id.created_at().unix_timestamp()
                        )),
                )
                .await
                .unwrap();
            }
            db::set(
                id.get(),
                (
                    content.clone(),
                    attachments.iter().map(|a| a.url.clone()).collect(),
                    author.get(),
                ),
            )
        }
        FullEvent::MessageDelete {
            deleted_message_id,
            channel_id,
            ..
        } => {
            let log = c
                .http()
                .get_guild(925674713429184564.into())
                .await
                .unwrap()
                .audit_logs(
                    c,
                    Some(audit_log::Action::Message(MessageAction::Delete)),
                    None,
                    None,
                    Some(1),
                )
                .await?
                .entries
                .into_iter()
                .next()
                .unwrap();

            let ban_log = c
                .http()
                .get_guild(925674713429184564.into())
                .await
                .unwrap()
                .audit_logs(
                    c,
                    Some(audit_log::Action::Member(MemberAction::BanAdd)),
                    None,
                    None,
                    Some(1),
                )
                .await?
                .entries
                .into_iter()
                .next()
                .unwrap()
                .id;

            let now = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs();
            let since = now.saturating_sub(log.id.created_at().unix_timestamp() as _);
            let since_ban = now.saturating_sub(ban_log.created_at().unix_timestamp() as _);
            if since_ban < 60 {
                return Ok(());
            }
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
                                if author.get() != a || since > 20 {
                                    format!(
                                        "<t:{}:d> <@{a}> {CANCEL} deleted their own message (in <#{channel_id}>):```\n{content}\n```\n{}",
                                        deleted_message_id.created_at().unix_timestamp(),
                                        links
                                            .into_iter()
                                            .reduce(|a, b| format!("{a} {b}"))
                                            .unwrap_or_else(String::new)
                                    )
                                } else {
                                    format!(
                                        "<t:{}:d> <@{who}> {CANCEL} deleted message by <@{author}> (in <#{channel_id}>):```\n{content}\n```\n{}",
                                        deleted_message_id.created_at().unix_timestamp(),
                                        links
                                            .into_iter()
                                            .reduce(|a, b| format!("{a} {b}"))
                                            .unwrap_or_else(String::new)
                                    )
                                }
                            }
                            None => format!(
                                "<t:{}:d> <@{who}> {CANCEL} deleted message by <@{author}> in <#{channel_id}> (content unavailable)",
                                deleted_message_id.created_at().unix_timestamp()
                            ),
                        }),
                )
                .await
                .unwrap();
        }
        _ => (),
    }
    Ok(())
}

pub struct Bot;
impl Bot {
    pub async fn spawn() {
        println!("bot startup");
        let tok =
            std::env::var("TOKEN").unwrap_or_else(|_| read_to_string("token").expect("wher token"));
        let f = poise::Framework::builder()
            .options(poise::FrameworkOptions {
                commands: vec![help(), reload(), redact(), spy()],
                on_error: |e| Box::pin(on_error(e)),
                event_handler: |c, e, _, _| Box::pin(event_handler(c, e)),
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

#[poise::command(
    slash_command,
    context_menu_command = "Read all messages",
    rename = "spy"
)]
/// Collect the messages of a user.
/// Will be dispatched to your DM's.
/// This command may take some time.
pub async fn spy(c: Context<'_>, #[description = "the user to spy on"] who: User) -> Result<()> {
    let u = c.author_member().await.ok_or(anyhow::anyhow!("dang"))?;
    if !(u.user.name == "bendn"
        || u.roles.contains(&RoleId::new(925676016708489227))
        || u.roles.contains(&RoleId::new(925708634896367647)))
    {
        poise::say_reply(c, "access denied. this incident will be reported").await?;
        return Ok(());
    }
    let h = c.reply("please check your dm's").await?;
    let mut n = 0u64;
    for (x, y) in db::values()
        .filter(|(_, _, x)| *x == who.id)
        .map(|(x, y, _)| (x, y))
    {
        if let Err(_) = c
            .author()
            .dm(
                c,
                CreateMessage::new().content(format!(
                    "```{x}```\n{}",
                    y.into_iter()
                        .reduce(|a, b| format!("{a} {b}"))
                        .unwrap_or_default()
                )),
            )
            .await
            && n == 0
        {
            h.edit(
                c,
                CreateReply::default().content("please open your dm's (couldnt send)"),
            )
            .await?;
            return Ok(());
        };
        n += 1;
    }
    h.edit(
        c,
        CreateReply::default().content(format!("all ({n}) sent. please check your dm's")),
    )
    .await?;
    Ok(())
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

#[poise::command(context_menu_command = "redact")]
/// Redact a log.
pub async fn redact(
    c: Context<'_>,
    #[description = "msg to redact"] mut message: Message,
) -> Result<()> {
    if message.is_own(c) {
        message
            .edit(
                c,
                EditMessage::default().content(format!("||[REDACTED] by <@{}>||", c.author().id)),
            )
            .await?;
        let h =
            poise::send_reply(c, poise::CreateReply::default().ephemeral(true).content(OK)).await?;

        tokio::time::sleep(Duration::from_secs(1)).await;
        _ = h.delete(c).await;
    } else {
        poise::say_reply(c, "access denied. this incident will be reported").await?;
    }
    Ok(())
}
