use yew::prelude::*;

pub enum TagsMsg {
    TagClick(MouseEvent, String)
}

#[derive(Debug)]
pub struct Tags {
    pub link: ComponentLink<Self>,
}

pub struct Props {
}

impl Tags {

}

impl Component for Tags {
    type Message = TagsMsg;
    type Properties = Props;

    fn create(props: Properties, link: ComponentLink<Self>) -> Self {
        Self {
            link,
        }
    }

    fn change(&mut self, _props: Properties) -> ShouldRender {
        false
    }

    fn update(&mut self, msg: Message) -> ShouldRender {
        false
    }

    fn view(&self) -> Html {
        unimplemented!()
    }

}
