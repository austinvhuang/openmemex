use yew::services::fetch::{FetchService, FetchTask, Request, Response};
use yew::{
    format::{Json, Nothing},
    prelude::*,
    utils::*,
};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;


#[derive(Deserialize, Debug, Clone)]
pub struct NoteResponse {
    pub code: i32,
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

#[derive(Serialize, Deserialize)]
struct Payload {
    #[serde(rename(serialize = "pnContent", deserialize="pnContent"))]
    note_content: String,
    #[serde(rename(serialize = "pnTags", deserialize="pnTags"))]
    tags: Vec<String>,
}

pub struct AddNote {
    content: String, // holds text in the note input
    tag: String, // holds text in the tag input
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
    fn change(&mut self, props: Self::Properties) -> bool {
        false
    }

    fn update(&mut self, msg: Self::Message) -> bool {
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
                self.content.truncate(self.content.len() - 1); // truncate trailing \n
                log::info!("self.content {:?}", self.content);
                log::info!("self.content.clone() {:?}", self.content.clone());
                log::info!("submit note: {:?}", self.content);
                let query = "http://localhost:3000/submit/note";
                let request = Request::post(query)
                    .body(Nothing)
                    .expect("Could not build request.");
                let callback = self.link.callback_once(
                    |response: Response<Json<Result<Vec<NoteResponse>, anyhow::Error>>>| {
                        let Json(data) = response.into_body();
                        AddNoteMsg::SubmitResponse(data)
                    },
                );
                self.content = String::from(""); // TODO - oninput callback still fires and we're left with a black note
                log::info!("request is {:?}", request);
                // let task = FetchService::fetch(request, callback).expect("failed to start request");
                // self.cache_task = Some(task);
                true
            }

            AddNoteMsg::SubmitResponse(data) => {
                log::info!("submitted");
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

                <input type="text" class="tag-input" placeholder="tag" id="tagInput"
                    value = { &self.tag }
                    oninput = { self.link.callback(move |e: InputData| AddNoteMsg::TagEdit(e.value)) }
                    onkeydown={ self.link.callback(move |e: KeyboardEvent| AddNoteMsg::TagKeyDown(e)) } />
                <p/>

                <textarea rows="8" class="note-input"  placeholder="Enter to submit" id="noteContent"
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
                        html!{ <div>{ curr_tag }</div> }
                    })
                }
                </div>

            </div>
        }
    }
}