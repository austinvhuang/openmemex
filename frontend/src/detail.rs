use serde::{Deserialize, Serialize};
use serde_json::json;
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
// use wasm_bindgen::JsCast;
use crate::ace::*;


#[derive(Deserialize, Debug, Clone)]
pub struct CompletedResponse {
    pub code: i64,
}

pub enum DetailMsg {
    CompletedChange(ChangeData),
    CompletedResponse(Result<Vec<CompletedResponse>, anyhow::Error>),
    GetCompleted,
    ReceiveCompleted(Result<Vec<bool>, anyhow::Error>),
}

pub struct Detail {
    pub link: ComponentLink<Self>,
    pub entry: Option<Cache>,
    pub ace_editor: Option<JsValue>,
    pub completed: bool,
    submit_task: Option<FetchTask>,
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
    if url.contains("www.youtube.com") && url.contains("watch?v=") {
        new_url = youtube_url(url);
        iframe_style = String::from("width:100%; height:50vh;");
    }
    log::info!("new url is {:?}", new_url);
    log::info!("style is {:?}", iframe_style);
    (new_url, iframe_style)
}

fn completed_checkbox(detail: &Detail) -> Html {
    html! {
        <div>
            {
                html! {
                    <div class="item">
                        <input type="checkbox" id="finished" name="finished" 
                         onchange = {
                             detail.link.callback(move |e: ChangeData| DetailMsg::CompletedChange(e))
                             }
                         checked = detail.completed
                        />
                    </div>
                }
            }
            <label style="height:10%; margin-left: 10px"> {" Completed"} </label>
        </div>
    }
}

impl Component for Detail {
    type Message = DetailMsg;
    type Properties = Props;
    // let callback = |buffer: JsValue| log::info!("ace callback");
    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        let cb = link.callback_once(|_: String| DetailMsg::GetCompleted);
        cb.emit("".to_string()); // TODO - what's the right way to handle a message without parameters
        log::info!("sent GetCompleted message");
        Self {
            link: link,
            entry: props.entry,
            ace_editor: None,
            completed: false,
            submit_task: None
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
                            
                            match &self.entry { 
                                None => {
                                    log::info!("completed checbox : entry_id: None");
                                    false
                                }
                                Some(e) => {
                                    log::info!("completed checbox : entry_id: {:?}", e.entry_id);
                                    log::info!("completed checbox : content_id: {:?}", e.content_id);
                                    let server = host().unwrap();
                                    let query = format!("http://{}/submit/completed", server);
                                    let payload = CompletedPayload {
                                        content_id: e.content_id,
                                        state: self.completed,
                                    };
                                    // TODO can we use payload to deserialize here instead?
                                    // currently payload value isn't used
                                    let body = json!({"pcContentID": payload.content_id,
                                                      "pcState": payload.state});
                                    let request = Request::post(query)
                                        .header("Content-Type", "application/json")
                                        .body(Json(&body))
                                        .expect("Could not build request.");
                                    let callback = self.link.callback_once(
                                        |response: Response<Json<Result<Vec<CompletedResponse>, anyhow::Error>>>| {
                                            let Json(data) = response.into_body();
                                            DetailMsg::CompletedResponse(data)
                                        },
                                    );
                                    let task = FetchService::fetch(request, callback).expect("failed to start request");
                                    self.submit_task = Some(task);
                                    false
                                }
                            }
                    }
                    _ =>{
                        log::info!("completed checkbox : not a value: {:?}", v);
                        false
                    }
                }
            }
            DetailMsg::CompletedResponse(d) => {
                log::info!("completed response {:?}", d);
                false
            }
            DetailMsg::GetCompleted => {
                match &self.entry { 
                    None => { log::info!("no entry value"); }
                    Some(e) => {
                        let server = host().unwrap();
                        let query = format!("http://{}/get/completed/{:?}", server,e.content_id).to_string();
                        log::info!("submitting get completed : {:?}", query);
                        let request = Request::get(&query)
                            .body(Nothing)
                            .expect("Could not build request.");
                        let callback = self.link.callback_once(
                            |response: Response<Json<Result<Vec<bool>, anyhow::Error>>>| {
                                let Json(data) = response.into_body();
                                DetailMsg::ReceiveCompleted(data)
                            },
                        );
                        let task = FetchService::fetch(request, callback).expect("failed to start request");
                        self.submit_task = Some(task);
                    }
                }
                false
            }
            DetailMsg::ReceiveCompleted(completed) => {
                log::info!("received : {:?}", completed);
                match completed {
                    Ok(result) => { 
                        self.completed = result[0]; // TODO clean this up to be safe
                        log::info!("completed is now: {:?}", self.completed);
                    }
                    Err(error) => {
                        log::info!("receive error, error is:");
                        log::info!("{}", &error.to_string());
                        // Some(error.to_string());
                    }
                }
                true
            }
        }
    }

    
    fn rendered(&mut self, first_render: bool) {
        log::info!("calling init_ace");
        // self.ace_editor = Some(init_ace());
        //
        // init_ace();
        // ace_manual_init();
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
        let title: String = match &self.entry {
            Some(entry) => entry.content.clone().unwrap_or("".to_string()),
            None => "".to_string(),
        };
        // TODO note_content branch on is url?
        let note_content = match &self.entry {
            Some(entry) => if entry.url.is_none() { title } 
                           else { ["# Notes on", &title].join(" ") },
            None => "No Entry Selected".to_string(),
        };
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
                        <Ace init_content=note_content.clone() id="ace_editor" height="90%" />
                        <p/>
                        <center>
                        { completed_checkbox(self) }
                        </center>
                    </div>
                </div>
            </div>
        }

    }
}
