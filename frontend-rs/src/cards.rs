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
                            // log::info!("{:#?} : item.", item);
                            let parsed = Url::parse(item.url.as_ref().unwrap_or(&"".to_owned()));

                            let mut thumbnail_file = match item.thumbnail_file.clone() {
                                Some(mut value) => {
                                    value.truncate(value.len() - 4);
                                    let suffix: &str = "_tn.png";
                                    value.push_str(suffix);
                                    value
                                },
                                Nothing => "".to_string()
                            };

                            html! {
                                <div class="card" onmouseover=self.link.callback(|m| { CardsMsg::CardMouseOver(m) })>
                                        { item.date.clone() }
                                    <hr/>
                                    <a href={ item.url.as_ref().unwrap_or(&"".to_owned()).clone() }>
                                    // <img src=thumbnail_file width="100%" style="height: 100px; overflow: hidden;"/>
                                    <center>
                                        <img src=thumbnail_file style="height: 200px; overflow: hidden;"/>
                                    </center>
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
        log::info!("Creating cards component");
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
