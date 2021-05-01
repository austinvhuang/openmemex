use serde::{Deserialize, Serialize};
use crate::api::*;
use yew::services::fetch::{FetchService, FetchTask, Request, Response};
use yew::{
    format::{Json, Nothing},
    prelude::*,
    utils::*,
};
use yew::Properties;
use wasm_bindgen::prelude::*;
use crate::external::*;
use wasm_bindgen::JsCast;

#[derive(Deserialize, Debug, Clone)]
pub struct CompletedResponse {
    pub code: i64,
}

pub enum DetailMsg {
    CompletedChange(ChangeData),
    SubmitResponse(Result<Vec<CompletedResponse>, anyhow::Error>),
}

pub struct Detail {
    pub link: ComponentLink<Self>,
    pub entry: Option<Cache>,
    pub ace_editor: Option<JsValue>,
    pub completed: bool,
    // TODO: get ace callback working
    // pub ace_callback: dyn Fn(JsValue) -> (),
}

#[derive(Properties, Clone)]
pub struct Props {
    pub entry: Option<Cache>,
}

fn youtube_url(url: String) -> String {
    log::info!("remapping url {:?}", url);
    let tokens:Vec<&str> = url.split("watch?v=").collect();
    // todo - 0..11 only works for ascii byte sized chars- make this more general
    let video_id = tokens[1].to_owned()[0..11].to_string(); 
    log::info!("{:?}", video_id);
    return format!("http://www.youtube.com/embed/{}", video_id);
}

fn iframeify_url(url: String) -> (String, String) {
    let mut new_url = url.clone();
    let mut iframe_style = String::from("width:100%; height:90vh;");
    log::info!("checking url {:?}", new_url);
    if (url.contains("www.youtube.com") && url.contains("watch?v=")) {
        new_url = youtube_url(url);
        iframe_style = String::from("width:100%; height:50vh;");
    }
    log::info!("new url is {:?}", new_url);
    log::info!("style is {:?}", iframe_style);
    (new_url, iframe_style)
}

impl Component for Detail {
    type Message = DetailMsg;
    type Properties = Props;
    // let callback = |buffer: JsValue| log::info!("ace callback");
    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            link: link,
            entry: props.entry,
            ace_editor: None,
            completed: false,
            // ace_callback: unimplemented!(),
        }
    }

    fn change(&mut self, props: Self::Properties) -> bool {
        log::info!("updated entry to {:?}", props.entry);
        self.entry = props.entry;
        true
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        match msg {
            DetailMsg::CompletedChange(v) => {
                log::info!("completed changed: {:?}", v);
                match v {
                    yew::events::ChangeData::Value(s) =>{
                        self.completed = !self.completed;
                        log::info!("completed checbox : change value: {:?}", s);
                        log::info!("completed checbox : state: {:?}", self.completed);
                        log::info!("completed checbox : entry_id: {:?}", 
                            match &self.entry { None => -1, Some(e) => e.entry_id });
                        let server = host();
                    }
                    _ =>{
                        log::info!("completed checkbox : not a value: {:?}", v);
                    }
                }
            }
            DetailMsg::SubmitResponse(d) => {
                log::info!("submit response");
            }
        };
        false
    }

    
    fn rendered(&mut self, first_render: bool) {
        log::info!("calling init_ace");
        // self.ace_editor = Some(init_ace());
        init_ace();
        log::info!("called init_ace");
    }

    fn view(&self) -> Html {
        let default = String::from("https://unsplash.it/600/800?random");
        log::info!("Screen {:?}", &self.entry);
        let src = match &self.entry {
            Some(entry) => entry.url.as_ref().unwrap_or(&default),
            None => &default,
        };
        let (src_mapped, iframe_style) = iframeify_url(src.to_string());
        log::info!("Screen {:?}", src);
        html! {
            <div>
                <div class="twocol-equal">
                    <div class="container shadow p-3 mb-5 bg-body rounded">
                        <iframe class="responsive-iframe shadow p-3 mb-5 bg-body rounded" 
                                sandbox="allow-same-origin allow-scripts allow-popups allow-forms"
                                src=src_mapped style=iframe_style/>
                    </div>
                    <div style="height:85vh" class="shadow p-3 mb-5 bg-body rounded">
                        <div id="editor" style="height:90%;">
                            {"# Notes"}
                        </div>
                        <p/>
                        <center>
                            <div class="item">
                                <input type="checkbox" id="finished" name="finished" 
                                 onchange = {
                                     self.link.callback(move |e: ChangeData| DetailMsg::CompletedChange(e))
                                     }
                                />
                                <label style="height:10%; margin-left: 10px"> {" Completed"} </label>
                            </div>
                        </center>
                    </div>
                </div>
            </div>
        }

    }
}
