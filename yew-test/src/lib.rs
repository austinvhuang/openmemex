use wasm_bindgen::prelude::*;
use serde::Deserialize;
use yew::{format::{Json, Nothing}, prelude::*};
use yew::services::fetch::{FetchService, FetchTask, Request, Response};

#[derive(Deserialize, Debug, Clone)]
pub struct Entry {
    time: String,
    entry_id: i32,
    content: String,
    date: String,
}

#[derive(Debug)]
pub enum Msg {
    GetEntries,
    ReceiveEntries(Result<Vec<Entry>, anyhow::Error>),
}

#[derive(Debug)]
pub struct App {
    fetch_task: Option<FetchTask>,
    entries: Option<Vec<Entry>>,
    link: ComponentLink<Self>,
    error: Option<String>,
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

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            fetch_task: None,
            entries: Some(Vec::<Entry>::new()),
            link,
            error: None,
        }
    }

    fn change(&mut self, _props: Self::Properties) -> bool {
        false
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        use Msg::*;

        match msg {
            GetEntries => {
                // define request
                log::info!("submitting request"); // TODO: logging doesn't work yet
                let request = Request::get("http://localhost:3000/all/entries")
                    .body(Nothing)
                    .expect("Could not build request.");
                // define callback
                let callback =
                    self.link
                        .callback(|response: Response<Json<Result<Vec<Entry>, anyhow::Error>>>| {
                            let Json(data) = response.into_body();
                            Msg::ReceiveEntries(data)
                        });
                // task
                let task = FetchService::fetch(request, callback).expect("failed to start request");
                self.fetch_task = Some(task);
                true // redraw page
            }
            Msg::ReceiveEntries(response) => {
                match response {
                    Ok(result) => {
                        log::info!("Update: {:#?}", result);
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
    wasm_logger::init(wasm_logger::Config::default());
    yew::start_app::<App>();
}
