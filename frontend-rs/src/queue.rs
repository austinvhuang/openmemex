use yew::prelude::*;
use yew::Properties;

pub enum QueueMsg {}

pub struct Queue {
    pub link: ComponentLink<Self>,
}

#[derive(Clone, Properties)]
pub struct Props {}

impl Component for Queue {
    type Message = QueueMsg;
    type Properties = Props;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self { link: link }
    }

    fn change(&mut self, props: Self::Properties) -> bool {
        false
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        false
    }

    fn view(&self) -> Html {
        html! {
            <div>
            {"test"}
            </div>
        }
    }
}
