use crate::api::*;
use yew::Properties;
use yew::services::fetch::{FetchService, FetchTask, Request, Response};
use yew::{
    format::{Json, Nothing},
    prelude::*,
    utils::host,
};

pub enum TimelineMsg {
    GetTimeline,
    ReceiveTimeline(Result<Vec<Timestamp>, anyhow::Error>),
    Hover(MouseEvent, String),
}

#[derive(Debug)]
pub struct Timeline {
    pub link: ComponentLink<Self>,
    pub events: Vec<Timestamp>,
    pub locations: Vec<f32>,
    time_coord: i32,
    task: Option<FetchTask>,
}

#[derive(Clone, Properties)]
pub struct Props {}

impl Component for Timeline {
    type Message = TimelineMsg;
    type Properties = Props;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {

        let server = host().unwrap();
        let cb = link.callback_once(|_: String| TimelineMsg::GetTimeline);
        cb.emit("".to_string());
        Self {
            link: link,
            time_coord: 0,
            events: [].to_vec(),
            locations: [].to_vec(),
            task: None,
        }
    }

    fn change(&mut self, props: Self::Properties) -> bool {
        false
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        use TimelineMsg::*;
        let server = host().unwrap();
        log::info!("update");
        match msg {
            GetTimeline => {
                let query = format!("http://{}/all/timestamps/", server.to_string());
                log::info!("submitting tl request: {}", query);
                let request = Request::get(&query)
                    .body(Nothing)
                    .expect("Could not build request.");
                let callback = self.link.callback_once(
                    |response: Response<Json<Result<Vec<Timestamp>, anyhow::Error>>>| {
                        let Json(data) = response.into_body();
                        TimelineMsg::ReceiveTimeline(data)
                    });
                let task = FetchService::fetch(request, callback).expect("failed to start request");
                self.task = Some(task);
                log::info!("submitting timeline request");
                false
            }
            ReceiveTimeline(response) => {
                match response {
                    Ok(result) => {
                        log::info!("timeline results {}", result.len());
                        self.events = result.clone();
                        let mut timestamps = vec![0; result.len()]; // Vec::with_capacity(result.len());
                        for (i, timestamp) in result.iter().enumerate() {
                            timestamps[i] = timestamp.utc
                        }
                        let min: f32 = (*timestamps.iter().min().unwrap_or(&0)) as f32;
                        let max: f32 = (*timestamps.iter().max().unwrap_or(&0)) as f32;
                        let rng = max - min;
                        self.locations = timestamps.into_iter().map(|x| (100.0 * ((x as f32) - min) / rng)).collect();
                        /*
                        for t in &self.locations {
                            log::info!("loc: {}", t);
                        }
                        */
                    }
                    Err(error) => {
                        log::info!("timeline error:");
                        log::info!("{:?}", &error.to_string());
                    }
                }
                true
            }
            Hover(m, s) => { 
                self.time_coord = m.offset_x();
                // log::info!("Timeline hover msg: {}", &s);
                // log::info!("Timeline hover mouse state: {} {}", &m.client_x(), &m.client_y());
                true 
            }
        }
    }

    fn view(&self) -> Html {
        let hover_callback = |input: String| {
            self.link
                .callback(move |m| TimelineMsg::Hover(m, input.clone()))
        };
        html! {
            <div>
                <svg height="50" width="100%" onmouseover=hover_callback("ma".to_string()) onmousemove=hover_callback("svg".to_string())>
                    <line x1="0%" y1="25" x2="100%" y2="25" style="stroke:rgb(0,0,0);stroke-width:2" onmouseover=hover_callback("line".to_string()) onmousemove=hover_callback("line".to_string())  />
                    <line x1="0%" y1="20" x2="0%" y2="30" style="stroke:rgb(0,0,0);stroke-width:4" />
                    <line x1="100%" y1="20" x2="100%" y2="30" style="stroke:rgb(0,0,0);stroke-width:4" />
                    <line x1=self.time_coord y1="0%" x2=self.time_coord y2="100%" style="stroke:rgb(0,0,0, 0.2);stroke-width:32" />
                    {
                        for self.locations.iter().map(move |loc| {
                            html! {
                                <circle cx=format!("{:.0}%", loc) cy="50%" r="3" style="stroke:rgb(0,0,0,0.1);fill:rgb(0,0,0,0.1);" />
                            }
                        })
                    }
                </svg>
            </div>
        }
    }
}

// <line x1=format!("{:.0}%", loc) y1="33%" x2=format!("{:.0}%", loc) y2="66%" style="stroke:rgb(0,0,0,0.2);stroke-width:2" />
