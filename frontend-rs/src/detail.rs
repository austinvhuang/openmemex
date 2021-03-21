use crate::api::*;
use crate::external::*;
use wasm_bindgen::prelude::*;
use yew::services::fetch::{FetchService, FetchTask, Request, Response};
use yew::Properties;
use yew::{
    format::{Json, Nothing},
    prelude::*,
    utils::*,
};

pub enum DetailMsg {}

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
        log::info!("updated entry to {:?}", props.entry);
        self.entry = props.entry;
        true
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        false
    }

    fn rendered(&mut self, first_render: bool) {
        log::info!("calling init_ace");
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
        log::info!("Screen {:?}", src);
        html! {
            <div>
                <div class="twocol-equal">
                    <div class="container shadow p-3 mb-5 bg-body rounded">
                        <iframe class="responsive-iframe shadow p-3 mb-5 bg-body rounded" sandbox="allow-same-origin allow-scripts allow-popups allow-forms"
                        src=src style="width:100%; height:90vh;"/>
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
