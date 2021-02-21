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
use crate::api::*;
use std::path::Path;
use std::collections::HashSet;

#[derive(Debug)]
pub enum CardsMsg {
    GetEntries,
    ReceiveEntries(Result<Vec<Cache>, anyhow::Error>),
    CardMouseOver(MouseEvent),
}


#[derive(Debug)]
pub struct Cards {
    pub cache_task: Option<FetchTask>,
    pub tag_task: Option<FetchTask>,
    pub entries: Option<Vec<Cache>>,
    selected_tags: HashSet<String>,
    pub link: ComponentLink<Self>,
    pub error: Option<String>,
    pub query: String,
}

impl Cards {
    fn view_entries(&self) -> Html {
        match self.entries {
            Some(ref entries) => {
                log::info!("{:#?} results fetched.", entries.len());
                html! {
                    {
                        for entries.iter().map(|mut item| {
                            // TODO - handle None for options
                            let parsed = Url::parse(item.url.as_ref().unwrap_or(&"".to_owned()));
                            let mut thumbnail_file = item.thumbnail_file.clone().unwrap_or("".to_owned());
                            let suffix: &str = "_tn.png";
                            thumbnail_file.truncate(thumbnail_file.len() - 4);
                            thumbnail_file.push_str(suffix); 
                            // TODO - replace prefix with thumbnails/ !!
                            log::info!("screenshot: {:?}", thumbnail_file);
                            log::info!("thumbnail: {:?}", thumbnail_file);
                            html! {
                                <div class="card" onmouseover=self.link.callback(|m| { CardsMsg::CardMouseOver(m) })>
                                    <h4>
                                        { item.date.clone() }
                                    </h4>
                                    <hr/>
                                    <a href={ item.url.as_ref().unwrap_or(&"".to_owned()).clone() }>
                                    <img src=thumbnail_file width="100%"/>
                                    </a>
                                    {
                                        match &parsed {
                                            Ok(x) => { x.host_str().unwrap() }
                                            Err(error) => { "" }
                                        }
                                    }
                                    <a href={ item.url.as_ref().unwrap_or(&"".to_owned()).clone() }>
                                        { item.content.clone().unwrap_or("".to_owned()) }
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
}

impl Component for Cards {

    type Message = CardsMsg;
    type Properties = ();

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        log::info!("Creating component");
        let cb = link.callback_once(|_: String| CardsMsg::GetEntries);
        cb.emit("".to_string()); // TODO - what's the right way to handle a message without parameters
        log::info!("sent message");
        Self {
            cache_task: None,
            tag_task: None,
            entries: None, //Some(Vec::<Entry>::new()),
            selected_tags: HashSet::new(),
            link,
            error: None,
            query: "http://localhost:3000/all/cache".to_string(),
        }
    }


    fn change(&mut self, _props: Self::Properties) -> bool {
        false
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        use CardsMsg::*;
        log::info!("update");
        match msg {
            GetEntries => {
                // define request
                log::info!("submitting cache request");
                let request = Request::get(&self.query)
                    .body(Nothing)
                    .expect("Could not build request.");
                // define callback
                let callback = self.link.callback_once(
                    |response: Response<Json<Result<Vec<Cache>, anyhow::Error>>>| {
                        let Json(data) = response.into_body();
                        CardsMsg::ReceiveEntries(data)
                    },
                );
                // task
                let task = FetchService::fetch(request, callback).expect("failed to start request");
                self.cache_task = Some(task);
                false // redraw page
            }
            CardsMsg::ReceiveEntries(response) => {
                match response {
                    Ok(result) => {
                        // log::info!("Update: {:#?}", result);
                        self.entries = Some(result);
                    }
                    Err(error) => {
                        log::info!("cache receive error, error is:");
                        log::info!("{}", &error.to_string());
                        self.error = Some(error.to_string());
                    }
                }
                self.cache_task = None;
                true
            }
            CardMouseOver(_m) => {
                log::info!("card mouseover event");
                false
            }
        }
    }


    fn view(&self) -> Html {
        html! {
                      <div class="cards">
                          { self.view_entries() }
                      </div>
        }
    }

}
