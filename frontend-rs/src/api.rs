use serde::Deserialize;
use url::*;
use wasm_bindgen::prelude::*;
use yew::events::*;
use yew::services::fetch::{FetchService, FetchTask, Request, Response};
use yew::{
    format::{Json, Nothing},
    prelude::*,
};
use yew_router::*;

#[derive(Deserialize, Debug, Clone)]
pub struct Entry {
    pub time: String,
    #[serde(rename(deserialize = "entryID"))]
    pub entry_id: i32,
    content: String,
    pub date: String,
}

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
    #[serde(rename(deserialize = "cvScreenshotFile"))]
    pub screenshot_file: Option<String>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct Tag {
    pub tag_name: String,
}
