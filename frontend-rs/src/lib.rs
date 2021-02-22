#![recursion_limit = "1024"]
// https://github.com/yewstack/yew/issues/513

mod api;
mod app;
mod cards;
mod post;

use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn run_app() {
    wasm_logger::init(wasm_logger::Config::default());
    yew::start_app::<app::App>();
}
