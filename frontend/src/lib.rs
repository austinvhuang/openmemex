#![recursion_limit = "1024"]
// https://github.com/yewstack/yew/issues/513
//
use wasm_bindgen::prelude::*;

mod ace;
mod add_note;
mod api;
mod app;
mod app_router;
mod cards;
mod detail;
mod external;
mod queue;
mod settings;
mod space;
mod tags;
mod timeline;

#[wasm_bindgen(start)]
pub fn run_app() {
    wasm_logger::init(wasm_logger::Config::default());
    yew::start_app::<app::App>();
}
