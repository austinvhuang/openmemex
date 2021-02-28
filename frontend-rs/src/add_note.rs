use yew::prelude::*;
use yew::services::fetch::{FetchService, FetchTask, Request, Response};

pub enum AddNoteMsg {
    SubmitNote(FocusEvent),
    EditNote(String),
    KeyDown(KeyboardEvent),
    AddTag(String),
}

pub struct AddNote {
    content: String,
    tags: Vec<String>,
    // ops: String,
    link: ComponentLink<Self>,
    submit_task: Option<FetchTask>
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
            AddNoteMsg::EditNote(content) => {
                log::info!("edit note {:?}", content);
                false
            },
            AddNoteMsg::KeyDown(keypress) => {
                log::info!("keydown {:?}", keypress);
                false
            },
            AddNoteMsg::AddTag(tag_name) => {
                log::info!("adding tag {:?}", tag_name);
                self.tags.push(tag_name);
                false
            },
            AddNoteMsg::SubmitNote(event) => {
                log::info!("submit {:?}", event);
                false
            }
        }
    }

fn view(&self) -> Html {
        html! {
            <div>
                <textarea rows="8" class="note-input" 
                    oninput={ self.link.callback(move |e: InputData| AddNoteMsg::EditNote(e.value)) }
                    onkeydown={ self.link.callback(move |e: KeyboardEvent| AddNoteMsg::KeyDown(e)) }
                    onsubmit={ self.link.callback(move |e: FocusEvent| AddNoteMsg::SubmitNote(e)) }
                    >
                </textarea>
            <input type="text" class="tag-input" placeholder="tag"/>
            </div>
        }
    }
}
