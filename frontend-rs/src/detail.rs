use crate::api::*;
use yew::services::fetch::{FetchService, FetchTask, Request, Response};
use yew::{
    format::{Json, Nothing},
    prelude::*,
    utils::*,
};
use yew::Properties;
use serde::{Deserialize, Serialize};
use serde_json::json;
use wasm_bindgen::prelude::*;

pub enum DetailMsg {

}

pub struct Detail {
    pub link: ComponentLink<Self>,
    pub entry: Option<Entry>,
}

#[derive(Properties, Clone)]
pub struct Props {
    pub entry: Option<Entry>,
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

    fn change(&mut self, _props: Self::Properties) -> bool {
        false
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        false
    }

    fn view(&self) -> Html {
        html! {
            <div>
                <h1>
                { "Detail" }
                </h1>
            </div>
        }

    }
}
