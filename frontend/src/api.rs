use serde::{Deserialize, Serialize};
use url::*;

#[derive(Deserialize, Debug, Clone)]
pub struct Event{
    pub time: String,
    #[serde(rename(deserialize = "entryID"))]
    pub entry_id: i32,
    // content: String,
    pub date: String,
}

// gallery view

#[derive(Deserialize, Debug, Clone)]
pub struct Cache {
    #[serde(rename(deserialize = "cvForeignID"))]
    pub entry_id: i32,
    #[serde(rename(deserialize = "cvContentID"))]
    pub content_id: i32,
    #[serde(rename(deserialize = "cvDate"))]
    pub date: String,
    #[serde(rename(deserialize = "cvTime"))]
    pub time: String,
    #[serde(rename(deserialize = "cvContent"))]
    pub content: Option<String>,
    #[serde(rename(deserialize = "cvUrl"))]
    pub url: Option<String>,
    #[serde(rename(deserialize = "cvDisplay"))]
    pub display: Option<String>, // TODO - should this be option or string?
    #[serde(rename(deserialize = "cvTitle"))]
    pub title: Option<String>,
    #[serde(rename(deserialize = "cvThumbnailFile"))]
    pub thumbnail_file: Option<String>,
    #[serde(rename(deserialize = "cvScreenshotFile"))]
    pub screenshot_file: Option<String>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct Tag {
    pub tag_name: String,
}

#[derive(Deserialize, Debug, Clone)]
pub struct Timestamp {
    #[serde(rename(deserialize = "dtDay"))]
    pub day: (i32, i32, i32),
    #[serde(rename(deserialize = "dtTimeOfDay"))]
    pub time_of_day: (i32, i32, i32),
    #[serde(rename(deserialize = "dtUTC"))]
    pub utc: i64
}

// add_note

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct AddNotePayload {
    #[serde(rename(serialize = "pnContent", deserialize = "pnContent"))]
    pub note_content: String,
    #[serde(rename(serialize = "pnTags", deserialize = "pnTags"))]
    pub tags: Vec<String>,
}

// detail

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct CompletedPayload {
    #[serde(rename(serialize = "pcContentID", deserialize = "pcContentID"))]
    pub content_id: i32,
    #[serde(rename(serialize = "pcState", deserialize = "pcState"))]
    pub state: bool,
}

// configuration
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Config {
    #[serde(rename(serialize = "showTagThresh", deserialize = "showTagThresh"))]
    pub show_tag_thresh: i32,
    #[serde(rename(serialize = "resultsPerPage ", deserialize = "resultsPerPage"))]
    pub results_per_page: i32,
    #[serde(rename(serialize = "port", deserialize = "port"))]
    pub port: i32,
    #[serde(rename(serialize = "dbFilename", deserialize = "dbFilename"))]
    pub db_filename: String,
}

pub struct CompletedResponse {
    pub code: i64,
}

pub fn is_url(txt: &str) -> bool {
    let result = Url::parse(txt);
    match &result {
        Ok(_) => true,
        Err(_) => false,
    }
}
