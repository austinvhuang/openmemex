use crate::api::*;
use url::*;
use yew::prelude::*;
use std::collections::HashMap;

#[derive(Debug)]
pub enum CardsMsg {
    CardMouseOver(MouseEvent, i32),
    CardClick(MouseEvent, i32, Cache),
}

#[derive(Debug)]
pub struct Cards {
    pub link: ComponentLink<Self>,
    pub entries: Option<Vec<Cache>>,
    pub entry_id_mouseover: Option<i32>,
    pub entry_id_click: Option<i32>,
}

#[derive(Clone, Properties)]
pub struct Props {
    pub entries: Option<Vec<Cache>>,
}

fn host_simplify(url: &str) -> String {
    // TODO - don't hard code this
    match url {
        "export.arxiv.org" => "Arxiv".to_string(),
        "www.arxiv.org" => "Arxiv".to_string(),
        "www.github.com" => "Github".to_string(),
        "medium.com" => "Medium".to_string(),
        "www.reddit.com" => "Reddit".to_string(),
        "twitter.com" => "Twitter".to_string(),
        "www.youtube.com" => "YouTube".to_string(),
        _ => url.to_string()
    }
}

impl Cards {
    fn view_card(&self, parsed: &Result<Url, url::ParseError>, thumbnail_file: &String, item: &Cache) -> Html {

        let img_style = "width: 70%;";
        let item_clone = item.clone();

        let img_class = if item.entry_id != self.entry_id_mouseover.unwrap_or(-1) {
            "card-img-background shadow-sm bg-white rounded"
        } else {
            "card-img-foreground shadow-sm bg-white rounded"
        };

        let div_class = if item.entry_id != self.entry_id_click.unwrap_or(-1) {
            if item.entry_id != self.entry_id_mouseover.unwrap_or(-1) {
                "card shadow p-3 mb-5 bg-body rounded"
            } else {
                "card shadow-lg p-3 mb-5 bg-white rounded"
            }
        } else {
            "card shadow-none p-3 mb-5 bg-light rounded"
        };

        let callback_mouseover = |entry_id| { 
            self.link.callback(move |m| { CardsMsg::CardMouseOver(m, entry_id) }) 
        };

        let callback_click = |entry_id| { 
            self.link.callback(move |m| { CardsMsg::CardClick(m, entry_id, item_clone.clone()) }) 
        };
        html! {
            <div class={ div_class } onmouseover=callback_mouseover(item.entry_id) onclick = callback_click(item.entry_id)>
                <center> { &item.date } </center>
                <hr/>
                // <img src=thumbnail_file width="100%" style="height: 100px; overflow: hidden;"/>
                <center>
                    <img src=thumbnail_file style=img_style class=img_class/>
                </center>
                <center>
                {
                    match &parsed {
                        Ok(x) => { host_simplify(x.host_str().unwrap()) }
                        Err(error) => { "".to_string() }
                    }
                }
                </center>

                {
                    match item.url.as_ref() {
                        Some(url) => html! {
                            <a href={ url.to_string() }> { item.content.clone().unwrap_or("".to_owned()) } </a>
                        },
                        None => html! {
                            { item.content.clone().unwrap_or("".to_owned()) }
                        }
                    }
                }

                /*
                <a href={ item.url.as_ref().unwrap_or(&"".to_owned()).clone() }>
                    { item.content.clone().unwrap_or("".to_owned()) }
                </a>
                */
            </div>
        }
    }

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
                            self.view_card(&parsed, &thumbnail_file, &item)
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
            entry_id_mouseover: None,
            entry_id_click: None,
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
            CardMouseOver(_m, entry_id) => {
                self.entry_id_mouseover = Some(entry_id);
                true
            }
            CardClick(_m, entry_id, item) => {
                // TODO
                log::info!("clicked {:?}", entry_id);
                if self.entry_id_click.unwrap_or(-1) != entry_id {
                    self.entry_id_click = Some(entry_id);
                } else {
                    self.entry_id_click = None;
                }
                true
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
