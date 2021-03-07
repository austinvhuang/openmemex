use yew::prelude::*;
use yew::Properties;

pub enum TagsMsg {
    TagClick(MouseEvent, String),
}

#[derive(Debug)]
pub struct Tags {
    pub link: ComponentLink<Self>,
    tags: Option<Vec<String>>,
    pub tag_click_callback: Callback<String>,
}

#[derive(Properties, PartialEq, Clone)]
pub struct Props {
    pub tags: Option<Vec<String>>,
    pub tag_click_callback: Callback<String>,
}

impl Tags {}

impl Component for Tags {
    type Message = TagsMsg;
    type Properties = Props;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        log::info!("Creating tags component");
        Self {
            link: link,
            tags: props.tags,
            tag_click_callback: props.tag_click_callback,
        }
    }

    fn change(&mut self, props: Self::Properties) -> ShouldRender {
        self.tags = props.tags;
        self.tag_click_callback = props.tag_click_callback;
        true
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        use TagsMsg::*;
        log::info!("tags update");
        match msg {
            TagClick(_m, tag_name) => {
                log::info!("tag click event");
                self.tag_click_callback.emit(tag_name);
                false
            }
        }
    }

    fn view(&self) -> Html {
        let empty_vec = &[].to_vec();
        let exist_tags = self.tags.as_ref().unwrap_or(empty_vec);
        let callback = |item: String| {
            self.link
                .callback(move |m| TagsMsg::TagClick(m, item.to_string()))
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
