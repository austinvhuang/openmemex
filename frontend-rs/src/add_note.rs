use yew::services::fetch::{FetchService, FetchTask, Request, Response};
use yew::{
    format::{Json, Nothing},
    prelude::*,
};
use serde::Deserialize;

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

pub struct AddNote {
    content: String,
    tags: Vec<String>,
    link: ComponentLink<Self>,
    submit_task: Option<FetchTask>,
}

impl Component for AddNote {
    type Message = AddNoteMsg;
    type Properties = ();

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            content: "Note goes here".to_string(),
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
                false
            }
            AddNoteMsg::TagKeyDown(keypress) => {
                log::info!("tag key down {:?}", keypress.key());
                if keypress.key() == "Enter" {
                    self.link.send_message(AddNoteMsg::AddTag("".to_string())); // todo save typed in tag
                }
                false
            }

            AddNoteMsg::SubmitNote => {
                log::info!("submit");
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
                // let task = FetchService::fetch(request, callback).expect("failed to start request");
                // self.cache_task = Some(task);
                false
            }

            AddNoteMsg::SubmitResponse(data) => {
                log::info!("note was submitted");
                false
            }


            AddNoteMsg::AddTag(tag_name) => {
                log::info!("adding tag {:?}", tag_name);
                self.tags.push(tag_name);
                false
            }

        }
    }

    fn view(&self) -> Html {
        html! {
            <div>
                <textarea rows="8" class="note-input" 
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
                <input type="text" class="tag-input" placeholder="tag" 
                    oninput = { self.link.callback(move |e: InputData| AddNoteMsg::TagEdit(e.value)) }
                    onkeydown={ self.link.callback(move |e: KeyboardEvent| AddNoteMsg::TagKeyDown(e)) } />
                <p/>
                <button type="submit" onclick=self.link.callback(move |e: MouseEvent| AddNoteMsg::SubmitNote)>
                { "Add Note" }
                </button>
            </div>
        }
    }
}
