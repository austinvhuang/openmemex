use crate::api::*;
use yew::services::fetch::{FetchService, FetchTask, Request, Response};
use yew::{
    format::{Json, Nothing},
    prelude::*,
    utils::*,
};
use yew::Properties;
use wasm_bindgen::prelude::*;

pub enum DetailMsg {

}

pub struct Detail {
    pub link: ComponentLink<Self>,
    pub entry: Option<Cache>,
}

#[derive(Properties, Clone)]
pub struct Props {
    pub entry: Option<Cache>,
}

fn iframeify_url(url: String) -> String {
    unimplemented!()
}

impl Component for Detail {
    type Message = DetailMsg;
    type Properties = Props;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            link: link,
            entry: props.entry,
        }
    }

    fn change(&mut self, props: Self::Properties) -> bool {
        self.entry = props.entry;
        true
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        false
    }

    fn view(&self) -> Html {
        let default = String::from("https://unsplash.it/1920/1080?random");
        log::info!("Screen {:?}", &self.entry);
        let src = match &self.entry {
            Some(entry) => entry.url.as_ref().unwrap_or(&default),
            None => &default,
        };
        log::info!("Screen {:?}", src);
        html! {
            <div>
                <div class="twocol">
                    <div>
                        <iframe sandbox="allow-same-origin allow-scripts allow-popups allow-forms"
                        src=src style="width:100%; height: 25vh;"/>
                    </div>
                    <div>
                    <input type="checkbox" id="finished" name="finished" value="finished"/>
                    <textarea rows="24" class="detail-input shadow-sm p-3 mb-5 bg-white rounded" 
                    placeholder="notes"/>
                    </div>
                </div>
            </div>
        }

    }
}
