#![recursion_limit = "1024"]
// https://github.com/yewstack/yew/issues/513

use serde::Deserialize;
use wasm_bindgen::prelude::*;
use yew::services::fetch::{FetchService, FetchTask, Request, Response};
use yew::{
    format::{Json, Nothing},
    prelude::*,
};
use yew::events::*;
use yew_router::*;

#[derive(Switch)]
enum AppRoute {
    #[to="/cards"]
    Cards,
    #[to="/screen"]
    Screen,
    #[to="/timeline"]
    Timeline,
    #[to="/addnote"]
    AddNote
}

#[derive(Deserialize, Debug, Clone)]
pub struct Entry {
    time: String,
    #[serde(rename(deserialize = "entryID"))]
    entry_id: i32,
    content: String,
    date: String,
}

#[derive(Deserialize, Debug, Clone)]
pub struct Cache {
    #[serde(rename(deserialize = "cvTime"))]
    time: String,
    #[serde(rename(deserialize = "cvForeignID"))]
    entry_id: i32,
    #[serde(rename(deserialize = "cvContent"))]
    content: String,
    #[serde(rename(deserialize = "cvDate"))]
    date: String,
    #[serde(rename(deserialize = "cvUrl"))]
    url: String,
}

#[derive(Debug)]
pub enum Msg {
    GetEntries,
    ReceiveEntries(Result<Vec<Cache>, anyhow::Error>),
    KeyDown,
    CardMouseOver(MouseEvent),
}

#[derive(Debug)]
pub struct App {
    fetch_task: Option<FetchTask>,
    entries: Option<Vec<Cache>>,
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
                                <div class="card" 
                                    onmouseover=
                                  self.link.callback(|m| { 
                                        Msg::CardMouseOver(m)
                                    })
                                >
                                    <h4>
                                        { item.date.clone() }
                                    </h4>
                                    <hr/>
                                    <a href={ item.url.clone() }>
                                    { item.content.clone() }
                                    </a>
                                </div>
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

    fn view_navbar(&self) -> Html {
        html! {
                <nav class="navbar navbar-expand-lg navbar-light bg-light">
                    <a class="navbar-brand" href="#">
                        { "note2self" }
                    </a>
                    <div class="collapse navbar-collapse" id="navbarNav">
                            <ul class="navbar-nav">
                                <li class="nav-item active">
                                    <a class="nav-link" href="#">{ "Cards"} </a>
                                </li>
                                <li class="nav-item">
                                    <a class="nav-link" href="#">{ "Screens" }</a>
                                </li>
                                <li class="nav-item">
                                    <a class="nav-link" href="#">{ "Timeline" }</a>
                                </li>
                                <li class="nav-item">
                                    <a class="nav-link" href="#">{ "Add Note" }</a>
                                </li>
                                <li class="nav-item">
                                    <a class="nav-link" href="#">{ "System" }</a>
                                </li>
                            </ul>
                        </div>
                    </nav>
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

        // let kb_cb = link.callback(Msg::KeyDown);
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
                let request = Request::get("http://localhost:3000/all/cache")
                    .body(Nothing)
                    .expect("Could not build request.");
                // define callback
                let callback = self.link.callback_once(
                    |response: Response<Json<Result<Vec<Cache>, anyhow::Error>>>| {
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
            KeyDown => {
                log::info!("keydown event");
                false
            }
            CardMouseOver(_m) => {
                log::info!("mouseover event");
                // log::info!(m);
                true
            }
        }
    }

    fn view(&self) -> Html {
        html! {
        <div class="main-outer"
            onkeydown={ self.link.callback(move |e: KeyboardEvent| {
            e.stop_propagation();
              Msg::KeyDown
        })}>
        { self.view_navbar() }

                <div class="main-inner">
                  <h1>
                      { "Note2Self" }
                  </h1>
                  <hr/>
                  <p/>
                  <input type="text" class= "search-input" placeholder="Search"/>
                  <p/>
                      <>
                          { self.view_entries() }
                      </>
                  </div>
              </div>
          }
    }
}

#[wasm_bindgen(start)]
pub fn run_app() {
    wasm_logger::init(wasm_logger::Config::default());
    yew::start_app::<App>();
}
