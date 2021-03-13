use crate::api::*;
use url::*;
use yew::prelude::*;

#[derive(Debug)]
pub enum CardsMsg {
    CardMouseOver(MouseEvent),
    CardClick(MouseEvent, i32),
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

impl Cards {
    fn view_card(&self, parsed: &Result<Url, url::ParseError>, thumbnail_file: &String, item: &Cache) -> Html {
        html! {
            <div class="card shadow p-3 mb-5 bg-body rounded" onmouseover=self.link.callback(|m| { CardsMsg::CardMouseOver(m) })>
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
                            self.view_card(&parsed, &thumbnail_file, &item);

                            html! {
                                <div class="card shadow p-3 mb-5 bg-body rounded" onmouseover=self.link.callback(|m| { CardsMsg::CardMouseOver(m) })>
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
            CardMouseOver(_m) => {
                log::info!("card mouseover event");
                false
            }
            CardClick(_m, entry_id) => {
                // TODO
                log::info!("clicked {:?}", entry_id);
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
