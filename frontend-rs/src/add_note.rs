use yew::prelude::*;

pub enum AddNoteMsg {
    SubmitNote,
}

pub struct AddNote {
    content: String,
    tags: Vec<String>,
    // ops: String,
    link: ComponentLink<Self>,
}

impl Component for AddNote {
    type Message = AddNoteMsg;
    type Properties = ();

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            content: "Note goes here".to_string(),
            tags: [].to_vec(),
            link: link,
        }
    }
    fn change(&mut self, props: Self::Properties) -> bool {
        false
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        false
    }

fn view(&self) -> Html {
        html! {
            <div>
                <textarea rows="8" class="note-input">
                </textarea>
            <input type="text" class="tag-input" placeholder="tag"/>
            </div>
        }
    }
}
