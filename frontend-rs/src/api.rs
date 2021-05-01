use serde::{Deserialize, Serialize};

#[derive(Deserialize, Debug, Clone)]
pub struct Entry {
    pub time: String,
    #[serde(rename(deserialize = "entryID"))]
    pub entry_id: i32,
    content: String,
    pub date: String,
}

// gallery view

#[derive(Deserialize, Debug, Clone)]
pub struct Cache {
    #[serde(rename(deserialize = "cvTime"))]
    pub time: String,
    #[serde(rename(deserialize = "cvForeignID"))]
    pub entry_id: i32,
    #[serde(rename(deserialize = "cvContent"))]
    pub content: Option<String>,
    #[serde(rename(deserialize = "cvDate"))]
    pub date: String,
    #[serde(rename(deserialize = "cvUrl"))]
    pub url: Option<String>,
    #[serde(rename(deserialize = "cvThumbnailFile"))]
    pub thumbnail_file: Option<String>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct Tag {
    pub tag_name: String,
}

// add_note

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Payload {
    #[serde(rename(serialize = "pnContent", deserialize = "pnContent"))]
    note_content: String,
    #[serde(rename(serialize = "pnTags", deserialize = "pnTags"))]
    tags: Vec<String>,
}
