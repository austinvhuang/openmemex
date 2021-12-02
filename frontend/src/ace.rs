use wasm_bindgen::prelude::*;
use yew::{
    format::{Json, Nothing},
    prelude::*,
    utils::*,
};

#[wasm_bindgen]
pub extern {
    pub fn test_ffi();
    pub fn ace_test();
    pub fn ace_edit(dom_id: &str) -> JsValue;
    pub fn ace_set_theme(editor: JsValue, theme: &str);
    pub fn ace_set_font_size(editor: JsValue, theme: &str);
    pub fn ace_set_mode(editor: JsValue, mode: &str);
    pub fn ace_set_keyboard_handler(editor: JsValue, handler: &str);
    pub fn ace_manual_init() -> JsValue;
    pub fn ace_get_document(editor: JsValue) -> JsValue;
    pub fn ace_get_line(editor: JsValue, row: i32) -> String;
    // pub fn ace_get_all_lines(editor: JsValue) -> Vec<str>;
}


pub fn ace_check() {
    test_ffi();
}

pub struct Ace {
    pub ace_object: Option<JsValue>,
    pub config: AceProperties,
    pub content: String,
}

#[derive(Properties, Clone)]
pub struct AceProperties {
    pub id: String,
    pub style: Option<String>,
    pub theme: Option<String>,
    pub init_content: Option<String>,
}

pub enum AceMsg {
    Init,
}

impl Component for Ace {
    type Message = ();
    type Properties = AceProperties ;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            ace_object: None,
            // ace_object: Some(ace_edit(&props.id.clone())),
            content: props.clone().init_content.unwrap_or("".to_string()).clone(),
            config: props.clone(),
        }

        // self.ace_object = Some(ace_edit(&self.config.dom_id));
    }


    fn rendered(&mut self, first_render: bool) {
        if first_render {
            let ace = ace_edit(&self.config.id);
            self.ace_object = Some(ace.clone());
            ace_set_theme(ace.clone(), "ace/theme/monokai");
            ace_set_mode(ace.clone(), "ace/mode/markdown");
            ace_set_font_size(ace.clone(), "20px");
            ace_set_keyboard_handler(ace.clone(), "ace/keyboard/vim");
        }
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        false
    }

    fn change(&mut self, _props: Self::Properties) -> bool {
        false
    }

    fn view(&self) -> Html {
        html! {
            // <div id=self.config.id.clone() style=self.config.style.clone()>
            <div id=self.config.id.clone() style="height:90%">
                { self.content.clone() }
            </div>
        }
    }
    // log::info!("{}", &ace_get_line(self.editor, 0)); // TODO make this work as intended
}
