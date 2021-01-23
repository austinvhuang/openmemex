use anyhow::Error;
use serde::Deserialize;
use wasm_bindgen::prelude::*;
use yew::format::{Json, Nothing};
use yew::services::fetch::{FetchService, FetchTask, Request, Response};
use yew::*;

pub struct App {
    fetch_task: Option<FetchTask>,
    entries: Option<Entries>,
    link: ComponentLink<Self>,
    error: Option<String>,
}

#[derive(Debug)]
pub enum Msg {
    GetEntries,
    ReceiveEntries(Result<Entries, anyhow::Error>),
}

#[derive(Deserialize, Debug, Clone)]
pub struct Entries {
    entries: Vec<Entry>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct Entry {
    time: String,
    entry_id: i32,
    content: String,
    date: String,
}

impl App {
    fn view_entries(&self) -> Html {
        html! { <div> {"Fetched"} </div> }
    }
    fn view_fetching(&self) -> Html {
        html! { <div> {"Fetching..."} </div> }
    }
    fn view_error(&self) -> Html {
        html! { <div> {"Error"} </div> }
    }
}

impl Component for App {
    type Message = Msg;
    type Properties = ();

    fn create(_: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            fetch_task: None,
            entries: None,
            link,
            error: None,
        }
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        match msg {
            GetEntries => {
                // define request
                let request = Request::get("http://localhost:3000/all/entires")
                    .body(Nothing)
                    .expect("Could not build request.");
                // define callback
                // TODO fix From trait error
                /*
                let callback =
                    self.link
                        .callback(|response: Response<Json<Result<Entries, anyhow::Error>>>| {
                            let Json(data) = response.into_body();
                            Msg::ReceiveEntries(data);
                        });
                // pass request+callback to fetch service
                let task = FetchService::fetch(request, callback).expect("failed to start request");
                self.fetch_task = Some(task);
                */
                true // redraw page
            }
            Msg::ReceiveEntries(response) => {
                match response {
                    Ok(result) => {
                        self.entries = Some(result);
                    }
                    Err(error) => {
                        self.error = Some(error.to_string());
                    }
                }
                self.fetch_task = None;
                true // redraw page
            }
        }
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        false
    }

    fn view(&self) -> Html {
        html! {
            <div class="main">
                <h1>
                    { "Note2Self" }
                </h1>
                <hr/>
                <p/>
                <input type="text" class= "search-input"/>
                <p/>
                <div class="card">
                    { "Hey" }
                </div>
                <div class="card">
                    { "There" }
                </div>
                    <>
                        { self.view_fetching() }
                        { self.view_entries() }
                        { self.view_error() }
                    </>
            </div>
        }
    }
}

#[wasm_bindgen(start)]
pub fn run_app() {
    yew::start_app::<App>();
}
