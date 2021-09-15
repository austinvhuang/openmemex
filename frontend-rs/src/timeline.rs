use crate::api::*;
use yew::Properties;
use yew::services::fetch::{FetchService, FetchTask, Request, Response};
use yew::{
    format::{Json, Nothing},
    prelude::*,
    utils::host,
};

use chrono::*;

use wasm_bindgen::prelude::*;

pub enum TimelineMsg {
    GetTimeline,
    ReceiveTimeline(Result<Vec<Timestamp>, anyhow::Error>),
    Hover(MouseEvent, String),
    Click(MouseEvent),
}

#[derive(Debug)]
pub struct Timeline {
    pub link: ComponentLink<Self>,
    pub events: Vec<Timestamp>,
    pub time_window: Option<(NaiveDateTime, NaiveDateTime)>,
    pub locations: Vec<f32>,
    pub time_coord: i32,
    pub utc_range: (i64, i64),
    pub timeline_callback: Callback<Option<(NaiveDateTime, NaiveDateTime)>>,
    task: Option<FetchTask>,
}

const window_half_duration: i64 = 3;

#[derive(Clone, Properties)]
pub struct Props {
    pub timeline_callback: Callback<Option<(NaiveDateTime, NaiveDateTime)>>,
}

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
            time_window: None,
            locations: [].to_vec(),
            utc_range: (0, 0),
            timeline_callback: props.timeline_callback,
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
                        let min = (*timestamps.iter().min().unwrap_or(&0)) as i64;
                        let max = (*timestamps.iter().max().unwrap_or(&0)) as i64;
                        // let (minf, maxf) = (min as f32, max as f32);
                        let minf = min as f32;
                        let maxf = max as f32;
                        self.locations = timestamps.into_iter().map(|x| 100.0 * (((x as f32) - minf) / (maxf - minf))).collect();
                        self.utc_range = (min, max);
                    }
                    Err(error) => {
                        log::info!("timeline error:");
                        log::info!("{:?}", &error.to_string());
                    }
                }
                true
            }
            Hover(m, _s) => { 
                self.time_coord = m.offset_x();
                let window = web_sys::window().expect("no global `window` exists");
                let document = window.document().expect("should have a document on window");
                let timeline = document.get_element_by_id("timeline-svg").expect("get element by id shouldn't fail");
                let width = timeline.client_width();
                let height = timeline.client_height();

                log::info!("Timeline hover mouse state: {} {}", &m.offset_x(), &m.offset_y());
                log::info!("Timeline hover width and height: {} {}", &width, &height);
                true 
            }
            Click(m) => {
                self.time_coord = m.offset_x();
                let window = web_sys::window().expect("no global `window` exists");
                let document = window.document().expect("should have a document on window");
                let timeline = document.get_element_by_id("timeline-svg").expect("get element by id shouldn't fail");
                let width = timeline.client_width();
                let height = timeline.client_height();

                let frac_position: f32 = (self.time_coord as f32) / (width as f32);
                let utc: i64 = (self.utc_range.0 as f32 + frac_position * (self.utc_range.1 - self.utc_range.0) as f32).round() as i64;

                let dt = NaiveDateTime::from_timestamp(utc, 0);

                // if adding / subtractiong the time window fails, clamp at the dt value
                let window_max = dt.checked_add_signed(Duration::days(window_half_duration))
                                   .unwrap_or(dt);
                let window_min = dt.checked_sub_signed(Duration::days(window_half_duration))
                                   .unwrap_or(dt);
                self.time_window = Some((window_min, window_max));

                self.timeline_callback.emit(Some((window_min, window_max)));

                // let dt: NaiveDateTime = NaiveDate::from_ymd(2016, 7, 8).and_hms(9, 10, 11);

                log::info!("Click Timeline mouse state: {} {}", &m.offset_x(), &m.offset_y());
                log::info!("Click Timeline width and height: {} {}", &width, &height);
                log::info!("Click Timeline fractional position: {}", &frac_position);
                log::info!("Click Timeline chrono: {}", &dt);
                log::info!("Click Timeline chrono min: {}", &window_min);
                log::info!("Click Timeline chrono max: {}", &window_max);
                true 

            }
        }
    }

    fn view(&self) -> Html {
        let hover_callback = |input: String| {
            self.link.callback(move |m| TimelineMsg::Hover(m, input.clone()))
        };
        let click_callback = || {
            self.link.callback(move |m| TimelineMsg::Click(m))
        };
        html! {
            <div>
                <svg height="50" width="100%" 
                    onmouseover=hover_callback("moousover".to_string()) 
                    onmousemove=hover_callback("hover".to_string()) 
                    onclick=click_callback()
                    id="timeline-svg">
                    <line x1="0%" y1="25" x2="100%" y2="25" style="stroke:rgb(0,0,0);stroke-width:2" onmouseover=hover_callback("line".to_string()) onmousemove=hover_callback("line".to_string())  />
                    <line x1="0%" y1="20" x2="0%" y2="30" style="stroke:rgb(0,0,0);stroke-width:4" />
                    <line x1="100%" y1="20" x2="100%" y2="30" style="stroke:rgb(0,0,0);stroke-width:4" />
                    <line x1=self.time_coord.to_string()  y1="0%" x2=self.time_coord.to_string() y2="100%" style="stroke:rgb(0,0,0, 0.2);stroke-width:32" />
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
