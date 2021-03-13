use std::collections::HashSet;
use yew::prelude::*;
use yew::Properties;

pub enum TagsMsg {
    TagClick(MouseEvent, String),
    TagHover(MouseEvent, String),
}

#[derive(Debug)]
pub struct Tags {
    pub link: ComponentLink<Self>,
    tags: Option<Vec<String>>,
    pub tag_click_callback: Callback<Option<String>>,
    pub selected: Option<HashSet<String>>,
    pub hovered: Option<String>,
}

#[derive(Properties, PartialEq, Clone)]
pub struct Props {
    pub tags: Option<Vec<String>>,
    pub tag_click_callback: Callback<Option<String>>,
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
            selected: None,
            hovered: None,
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
                let hs: HashSet<String> = vec![tag_name.clone()].into_iter().collect();
                let curr: HashSet<String> = self.selected.clone().unwrap_or(HashSet::new());
                if curr.contains(&tag_name) {
                    self.selected = None;
                    self.tag_click_callback.emit(None);
                } else {
                    self.selected = Some(hs);
                    self.tag_click_callback.emit(Some(tag_name));
                }
                true
            }
            TagHover(_m, tag_name) => {
                log::info!("tag hover event");
                self.hovered = Some(tag_name);
                true
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
        let hover_callback = |item: String| {
            self.link
                .callback(move |m| TagsMsg::TagHover(m, item.to_string()))
        };

        let hovered = self.hovered.clone().unwrap_or("".to_string());

        html! {
                <div class="topic-tags">
                    <div>
                        { for exist_tags.iter().map((move |item: &String| {
                            let hs = self.selected.clone().unwrap_or(HashSet::new());
                            if hs.contains(item) {
                                log::info!("{:?}", item);
                                 html! {
                                    <div class="topic-tag-selected" onclick=callback(item.clone()).clone()>
                                    { item.clone() }
                                    </div>
                                }
                            } else {
                                if (hovered.eq(item)) {
                                 html! {
                                    <div class="topic-tag-hover" onclick=callback(item.clone()).clone() onmouseover=hover_callback(item.clone()).clone()>
                                        { item.clone() }
                                    </div>
                                    }
                                } else {
                                 html! {
                                    <div class="topic-tag" onclick=callback(item.clone()).clone() onmouseover=hover_callback(item.clone()).clone()>
                                        { item.clone() }
                                    </div>
                                    }
                                }
                            }
                         }).clone() )
                        }
                    </div>
            </div>
        }
    }
}
