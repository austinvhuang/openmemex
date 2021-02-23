use yew::prelude::*;
use yew::Properties;

pub enum TagsMsg {
    TagClick(MouseEvent, String)
}

#[derive(Debug)]
pub struct Tags {
    pub link: ComponentLink<Self>,
}

#[derive(Properties, PartialEq, Clone)]
pub struct Props {
}

impl Tags {
}

impl Component for Tags {
    type Message = TagsMsg;
    type Properties = Props;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            link,
        }
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        false
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        false
    }

    fn view(&self) -> Html {
        unimplemented!()
    }

}
