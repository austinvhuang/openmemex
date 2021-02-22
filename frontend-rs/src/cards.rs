use crate::api::*;
use url::*;
use yew::prelude::*;

#[derive(Debug)]
pub enum CardsMsg {
    CardMouseOver(MouseEvent),
}

#[derive(Debug)]
pub struct Cards {
    pub link: ComponentLink<Self>,
    pub entries: Option<Vec<Cache>>,
}

#[derive(Clone, Properties)]
pub struct Props {
    pub entries: Option<Vec<Cache>>,
}

impl Cards {
    fn view_entries(&self) -> Html {
        match self.entries {
            Some(ref entries) => {
                log::info!("{:#?} results fetched.", entries.len());
                html! {
                    {
                        for entries.iter().map(|mut item| {
                            // TODO - handle None for options
                            let parsed = Url::parse(item.url.as_ref().unwrap_or(&"".to_owned()));
                            let mut thumbnail_file = item.thumbnail_file.clone().unwrap_or("".to_owned());
                            let suffix: &str = "_tn.png";
                            thumbnail_file.truncate(thumbnail_file.len() - 4);
                            thumbnail_file.push_str(suffix);
                            // TODO - replace prefix with thumbnails/ !!
                            log::info!("screenshot: {:?}", thumbnail_file);
                            log::info!("thumbnail: {:?}", thumbnail_file);
                            html! {
                                <div class="card" onmouseover=self.link.callback(|m| { CardsMsg::CardMouseOver(m) })>
                                    <h4>
                                        { item.date.clone() }
                                    </h4>
                                    <hr/>
                                    <a href={ item.url.as_ref().unwrap_or(&"".to_owned()).clone() }>
                                    <img src=thumbnail_file width="100%"/>
                                    </a>
                                    {
                                        match &parsed {
                                            Ok(x) => { x.host_str().unwrap() }
                                            Err(error) => { "" }
                                        }
                                    }
                                    <a href={ item.url.as_ref().unwrap_or(&"".to_owned()).clone() }>
                                        { item.content.clone().unwrap_or("".to_owned()) }
                                    </a>
                                </div>
                            }
                        })
                    }
                }
            }
            None => {
                html! { <div> {"No Content"} </div> }
            }
        }
    }
}

impl Component for Cards {
    type Message = CardsMsg;
    type Properties = Props;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        log::info!("Creating component");
        Self {
            link,
            entries: props.entries,
        }
    }

    fn change(&mut self, props: Self::Properties) -> bool {
        self.entries = props.entries;
        true
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        use CardsMsg::*;
        log::info!("update");
        match msg {
            CardMouseOver(_m) => {
                log::info!("card mouseover event");
                false
            }
        }
    }

    fn view(&self) -> Html {
        html! {
                      <div class="cards">
                          { self.view_entries() }
                      </div>
        }
    }
}
