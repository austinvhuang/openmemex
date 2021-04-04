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

pub enum DetailMsg {

}

pub struct Detail {
    pub link: ComponentLink<Self>,
    pub entry: Option<Cache>,
    pub ace_editor: Option<JsValue>,
    // pub ace_callback: dyn Fn(JsValue) -> (),
    pub ace_callback: JsValue,
}

#[derive(Properties, Clone)]
pub struct Props {
    pub entry: Option<Cache>,
}

fn iframeify_url(url: String) -> (String, String) {
    let mut new_url = url.clone();
    let mut iframe_style = String::from("width:100%; height:90vh;");
    log::info!("checking url {:?}", new_url);
    if (url.contains("www.youtube.com") && url.contains("watch?v=")) {
        log::info!("remapping url {:?}", new_url);
        let tokens:Vec<&str> = url.split("watch?v=").collect();
        // let video_id = tokens[1].to_owned().slice(0..11).to_string()
        let video_id = tokens[1].to_owned()[0..11].to_string(); // todo - 0..11 only works for ascii byte sized chars- make this more general
        log::info!("{:?}", video_id);
        new_url = format!("http://www.youtube.com/embed/{}", video_id);
        iframe_style = String::from("width:100%; height:50vh;");
    }
    log::info!("new url is {:?}", new_url);
    log::info!("style is {:?}", iframe_style);
    (new_url, iframe_style)
}

impl Component for Detail {
    type Message = DetailMsg;
    type Properties = Props;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            link: link,
            entry: props.entry,
            ace_editor: None,
            ace_callback: unimplemented!(), // |buffer: JsValue| log::info!("ace callback") ,
        }
    }

    fn change(&mut self, props: Self::Properties) -> bool {
        log::info!("updated entry to {:?}", props.entry);
        self.entry = props.entry;
        true
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        false
    }

    
    fn rendered(&mut self, first_render: bool) {
        log::info!("calling init_ace");
        // self.ace_editor = Some(init_ace());
        init_ace();
        log::info!("called init_ace");
    }

    fn view(&self) -> Html {
        let default = String::from("https://unsplash.it/1920/1080?random");
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
                        <iframe class="responsive-iframe shadow p-3 mb-5 bg-body rounded" sandbox="allow-same-origin allow-scripts allow-popups allow-forms"
                        src=src_mapped style=iframe_style/>
                    </div>
                    <div style="height:85vh" class="shadow p-3 mb-5 bg-body rounded">
                        <div id="editor" style="height:90%;">
                            {"# Notes"}
                        </div>
                        <p/>
                        <center>
                            <div class="item">
                                <input type="checkbox" id="finished" name="finished"/>
                                <label style="height:10%; margin-left: 10px">{" Completed"}</label>
                            </div>
                        </center>
                    </div>
                </div>
            </div>
        }

    }
}
