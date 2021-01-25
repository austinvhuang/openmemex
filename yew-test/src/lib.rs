use serde::Deserialize;
use wasm_bindgen::prelude::*;
use yew::services::fetch::{FetchService, FetchTask, Request, Response};
use yew::{
    format::{Json, Nothing},
    prelude::*,
};

#[derive(Deserialize, Debug, Clone)]
pub struct Entry {
    time: String,
    #[serde(rename(deserialize = "entryID"))]
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
        match self.entries {
            Some(ref entries) => {
                log::info!("{:#?} results fetched.", entries.len());
                // log::info!("Fetched ... {:#?}", entries);
                // html! { <div> {"Fetched"} </div> }
                html! {
                    {
                        for entries.iter().map(|mut item| {
                            html! {
                                <div class="card"> { item.content.clone() } </div>
                            }
                        })
                    }
                }
            }
            None => {
                html! { <div> {"No Content"} </div> }
            }
        }
    }
}

impl Component for App {
    type Message = Msg;
    type Properties = ();

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        log::info!("Creating component");
        let cb = link.callback_once(|_: String| Msg::GetEntries);
        cb.emit("".to_string()); // TODO - what's the right way to handle a message without parameters
        log::info!("sent message");
        Self {
            fetch_task: None,
            entries: None, //Some(Vec::<Entry>::new()),
            link,
            error: None,
        }
    }

    fn change(&mut self, _props: Self::Properties) -> bool {
        false
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        use Msg::*;
        log::info!("update");

        match msg {
            GetEntries => {
                // define request
                log::info!("submitting request"); // TODO: logging doesn't work yet
                let request = Request::get("http://localhost:3000/all/entries")
                    .body(Nothing)
                    .expect("Could not build request.");
                // define callback
                let callback = self.link.callback_once(
                    |response: Response<Json<Result<Vec<Entry>, anyhow::Error>>>| {
                        let Json(data) = response.into_body();
                        Msg::ReceiveEntries(data)
                    },
                );
                // task
                let task = FetchService::fetch(request, callback).expect("failed to start request");
                self.fetch_task = Some(task);
                false // redraw page
            }
            Msg::ReceiveEntries(response) => {
                match response {
                    Ok(result) => {
                        // log::info!("Update: {:#?}", result);
                        self.entries = Some(result);
                    }
                    Err(error) => {
                        log::info!("receive error, error is:");
                        log::info!("{}", &error.to_string());
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
                        { self.view_entries() }
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
