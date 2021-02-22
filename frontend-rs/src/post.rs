use yew::prelude::*;

pub enum PostMsg {
    SubmitNote,
}

pub struct Post {
    content: String,
    tags: String,
    // ops: String,
    link: ComponentLink<Self>,
}

impl Component for Post {
    type Message = PostMsg;
    type Properties = ();

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        unimplemented!()
    }
    fn change(&mut self, props: Self::Properties) -> bool {
        true
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        true
    }

    fn view(&self) -> Html {
        html! {
        <div>
        { "Hello" }
        </div>
                }
    }
}
