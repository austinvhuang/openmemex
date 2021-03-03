use yew::prelude::*;
use yew::services::fetch::{FetchService, FetchTask, Request, Response};

pub enum AddNoteMsg {

    NoteEdit(String),
    NoteKeyDown(KeyboardEvent),
    SubmitNote,

    TagEdit(String),
    TagKeyDown(KeyboardEvent),
    AddTag(String),

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
                log::info!("note keydown {:?}", keypress);
                false
            }

            AddNoteMsg::TagEdit(content) => {
                log::info!("tag edit {:?}", content);
                false
            }
            AddNoteMsg::TagKeyDown(keypress) => {
                log::info!("tag key down {:?}", keypress);
                false
            }

            AddNoteMsg::SubmitNote => {
                log::info!("submit");
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
                    onkeydown={ self.link.callback(move |e: KeyboardEvent| AddNoteMsg::NoteKeyDown(e)) }
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
