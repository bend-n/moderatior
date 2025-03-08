// TODO pruning?
use std::sync::LazyLock;

use kv::*;

fn cfg() -> kv::Config {
    kv::Config {
        path: "./db1".into(),
        temporary: false,
        use_compression: true,
        flush_every_ms: None,
        cache_capacity: None,
        segment_size: None,
    }
}
static DB: LazyLock<Store> = LazyLock::new(|| Store::new(cfg()).unwrap());
static BU: LazyLock<Bucket<Integer, Bincode<(String, Vec<String>, u64)>>> =
    LazyLock::new(|| DB.bucket(None).unwrap());

pub fn set(k: u64, v: (String, Vec<String>, u64)) {
    BU.set(&k.into(), &Bincode(v)).unwrap();
}
pub fn get(k: u64) -> Option<(String, Vec<String>, u64)> {
    BU.get(&k.into()).unwrap().map(|x| x.0)
}
pub fn sz() -> f32 {
    DB.size_on_disk().unwrap() as f32 / (1 << 20) as f32
}
pub fn set_m(m: poise::serenity_prelude::Message) {
    set(
        m.id.get(),
        (
            m.content,
            m.attachments
                .into_iter()
                .map(|x| x.url)
                .collect::<Vec<String>>(),
            m.author.id.into(),
        ),
    )
}
