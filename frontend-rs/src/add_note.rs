use serde::{Deserialize, Serialize};
use serde_json::json;
use wasm_bindgen::prelude::*;
use yew::services::fetch::{FetchService, FetchTask, Request, Response};
use yew::{
    format::{Json, Nothing},
    prelude::*,
    utils::*,
};

#[derive(Deserialize, Debug, Clone)]
pub struct NoteResponse {
    pub code: i64,
}

#[derive(Debug)]
pub enum AddNoteMsg {
    NoteEdit(String),
    NoteKeyDown(KeyboardEvent),
    SubmitNote,

    TagEdit(String),
    TagKeyDown(KeyboardEvent),
    AddTag(String),

    SubmitResponse(Result<Vec<NoteResponse>, anyhow::Error>),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Payload {
    #[serde(rename(serialize = "pnContent", deserialize = "pnContent"))]
    note_content: String,
    #[serde(rename(serialize = "pnTags", deserialize = "pnTags"))]
    tags: Vec<String>,
}

pub struct AddNote {
    content: String, // holds text in the note input
    tag: String,     // holds text in the tag input
    tags: Vec<String>,
    link: ComponentLink<Self>,
    submit_task: Option<FetchTask>,
}

impl Component for AddNote {
    type Message = AddNoteMsg;
    type Properties = ();

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            content: "".to_string(),
            tag: String::from(""),
            tags: [].to_vec(),
            link: link,
            submit_task: None,
        }
    }
    fn change(&mut self, _props: Self::Properties) -> bool {
        false
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        let server = host().unwrap();
        match msg {
            AddNoteMsg::NoteEdit(content) => {
                log::info!("note edit {:?}", content);
                self.content = content;
                false
            }
            AddNoteMsg::NoteKeyDown(keypress) => {
                log::info!("note keydown {:?}", keypress.key());
                if keypress.key() == "Enter" {
                    self.link.send_message(AddNoteMsg::SubmitNote);
                }
                false
            }

            AddNoteMsg::TagEdit(content) => {
                log::info!("tag edit {:?}", content);
                self.tag = content;
                false
            }

            AddNoteMsg::TagKeyDown(keypress) => {
                log::info!("tag key down {:?}", keypress.key());
                if keypress.key() == "Enter" {
                    log::info!("self.tag {:?}", self.tag);
                    log::info!("self.tag.clone() {:?}", self.tag.clone());
                    self.link.send_message(AddNoteMsg::AddTag(self.tag.clone())); // todo save typed in tag

                    log::info!("reset input element");
                    self.tag = String::from("");
                }
                true
            }

            AddNoteMsg::SubmitNote => {
                // self.content.truncate(self.content.len() - 1); // truncate trailing \n (... note
                // this doesn't
                // work since the submitnote triggers before editnote)
                log::info!("self.content {:?}", self.content);
                log::info!("self.content.clone() {:?}", self.content.clone());
                log::info!("submit note: {:?}", self.content);
                let query = format!("http://{}/submit/note", server);
                let payload = Payload {
                    note_content: self.content.clone(),
                    tags: self.tags.clone(),
                };
                let body = json!({"pnContent": self.content.clone(),
                                       "pnTags": self.tags.clone()});
                let request = Request::post(query)
                    .header("Content-Type", "application/json")
                    .body(Json(&body))
                    .expect("Could not build request.");
                log::info!("request is {:?}", request);

                let callback = self.link.callback_once(
                    |response: Response<Json<Result<Vec<NoteResponse>, anyhow::Error>>>| {
                        let Json(data) = response.into_body();
                        AddNoteMsg::SubmitResponse(data)
                    },
                );
                self.content = String::from(""); // TODO - oninput callback still fires and we're left with a black note
                log::info!("request payload {:?}", payload);
                let task = FetchService::fetch(request, callback).expect("failed to start request");
                self.submit_task = Some(task);
                true
            }

            AddNoteMsg::SubmitResponse(data) => {
                log::info!("submitted, response code received {:?}", data);
                true
            }

            AddNoteMsg::AddTag(tag_name) => {
                log::info!("adding tag {:?}", tag_name);
                self.tags.push(tag_name);
                log::info!("tag list: {:?}", self.tags);
                true
            }
        }
    }

    fn view(&self) -> Html {
        html! {
            <div>

                <input type="text" class="tag-input shadow-sm p-3 mb-5 bg-white rounded" placeholder="tag" id="tagInput"
                    value = { &self.tag }
                    oninput = { self.link.callback(move |e: InputData| AddNoteMsg::TagEdit(e.value)) }
                    onkeydown= { self.link.callback(move |e: KeyboardEvent| AddNoteMsg::TagKeyDown(e)) }
                />
                <p/>

                <textarea rows="8" class="note-input shadow-sm p-3 mb-5 bg-white rounded"  placeholder="note" id="noteContent"
                    value = { &self.content }
                    oninput={ self.link.callback(move |e: InputData| AddNoteMsg::NoteEdit(e.value)) }
                    onkeydown={ self.link.batch_callback(move
                        |e: KeyboardEvent|
                            if e.key() == "Enter" {
                                vec![AddNoteMsg::NoteKeyDown(e)]
                            } else {
                                vec![]
                            }) }
                    onsubmit={ self.link.callback(move |e: FocusEvent| AddNoteMsg::SubmitNote) }>
                </textarea>

                <div>
                {
                    for self.tags.iter().map(|mut curr_tag| {
                        html!{ <div class="topic-tag">{ curr_tag }</div> }
                    })
                }
                </div>

            </div>
        }
    }
}
