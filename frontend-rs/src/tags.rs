use yew::prelude::*;
use yew::Properties;
use yew::events::*;

pub enum TagsMsg {
    TagClick(MouseEvent, String)
}

#[derive(Debug)]
pub struct Tags {
    pub link: ComponentLink<Self>,
    tags: Option<Vec<String>>,
}

#[derive(Properties, PartialEq, Clone)]
pub struct Props {
    tags: Option<Vec<String>>,
}

impl Tags {
}

impl Component for Tags {
    type Message = TagsMsg;
    type Properties = Props;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        log::info!("Creating tags component");
        Self {
            link: link,
            tags: props.tags,
        }
    }

    fn change(&mut self, props: Self::Properties) -> ShouldRender {
        self.tags = props.tags;
        true
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        false
    }

    fn view(&self) -> Html {
        let empty_vec = &[].to_vec();
        let exist_tags = self.tags.as_ref().unwrap_or(empty_vec);
        let callback = |item: String| {
            self.link
                .callback(move |m| TagsMsg::TagClick(m, item.to_string().to_string()))
        };

        html! {
            <div class="topic-tags">
                { html! {
                <div>
                    { for exist_tags.iter().map((|item: &String| { html! {
                    <div class="topic-tag" onclick=callback(item.clone()).clone()>
                        { item.clone() }
                    </div>
                    } }).clone() ) }
                </div>
                } }
            </div>
        }
    }

}
