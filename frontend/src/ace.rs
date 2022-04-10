use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use yew::{
    prelude::*,
};

#[wasm_bindgen]
extern {
    pub fn test_ffi();
    pub fn ace_test();
    pub fn ace_edit(dom_id: &str) -> JsValue;
    pub fn ace_set_theme(editor: JsValue, theme: &str);
    pub fn ace_set_font_size(editor: JsValue, theme: &str);
    pub fn ace_set_mode(editor: JsValue, mode: &str);
    pub fn ace_set_keyboard_handler(editor: JsValue, handler: &str);
    pub fn ace_manual_init() -> JsValue;
    pub fn ace_get_document(editor: JsValue) -> JsValue;
    pub fn ace_get_line(editor: JsValue, row: u32) -> String;
    // pub fn ace_get_all_lines(editor: JsValue) -> JsValue; // TODO: extract vector from JsValue
    pub fn ace_get_length(editor: JsValue) -> u32;
    pub fn ace_add_callback(editor: JsValue, callback: &Closure<FnMut()>);
    pub fn ace_set_show_gutter(editor: JsValue, show_status: bool);
}

pub struct Ace {
    pub ace_object: Option<JsValue>,
    pub config: AceProperties,
    pub content: String,
    pub cb_handle: Option<Closure<dyn FnMut()>>,
    pub link: ComponentLink<Self>,
    defaults: Defaults,
    pub props: AceProperties,

}

#[derive(Properties, Clone)]
pub struct AceProperties {
    pub id: String,
    pub height: String,
    pub theme: Option<String>,
    pub mode: Option<String>,
    pub init_content: Option<String>,
    pub change_callback: Callback<Option<String>>,
}

#[derive(Properties, Clone)]
struct Defaults {
    pub theme: String,
    pub mode: String,
    pub init_content: String,
}


#[derive(Debug)]
pub enum AceMsg {
    Changed,
}

impl Component for Ace {
    type Message = AceMsg;
    type Properties = AceProperties;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        let defaults = Defaults {
            theme: "ace/theme/monokai".to_string(),
            mode: "ace/mode/markdown".to_string(),
            init_content: "".to_string(),
        };

        Self {
            ace_object: None,
            // ace_object: Some(ace_edit(&props.id.clone())),
            content: props.clone().init_content.unwrap_or("".to_string()).clone(),
            config: props.clone(),
            cb_handle: None,
            link: link,
            defaults: defaults,
            props: props,
        }
    }


    fn rendered(&mut self, first_render: bool) {
        if first_render {
            let ace = ace_edit(&self.config.id);
            self.ace_object = Some(ace.clone());
            ace_set_theme(ace.clone(),
                &self.props.theme.clone().unwrap_or(self.defaults.theme.clone()));
            ace_set_mode(ace.clone(),
                &self.props.mode.clone().unwrap_or(self.defaults.mode.clone()));
            ace_set_font_size(ace.clone(), "20px");
            ace_set_keyboard_handler(ace.clone(), "ace/keyboard/vim");
            ace_set_show_gutter(ace.clone(), false);
            let link = self.link.clone();
            let cb = Box::new(move || {
                let self_cb = link.callback_once(|_: String| AceMsg::Changed);
                self_cb.emit("".to_string());
                ()
            }) as Box<dyn FnMut()>;
                
            let closure = Closure::wrap(cb);
            ace_add_callback(ace.clone(), &closure);
            self.cb_handle = Some(closure);
        }
    }

    fn update(&mut self, msg: Self::Message) -> bool {
        match msg {
            AceMsg::Changed => {
                log::info!("Yew callback Message:");
                let ace = self.ace_object.clone().unwrap(); // TODO: exce3ptiono
                let length: u32 = ace_get_length(ace.clone());
                let mut vec: Vec<String> = vec!["".to_string(); (length as usize)];
                for row in 0..length {
                    vec[row as usize] = ace_get_line(ace.clone(), row);
                }
                let content = &vec.join("\n");
                log::info!("\nReading Content from Rust:\n{}\n----", content);
                self.props.change_callback.emit(Some(content.clone()));
            }

        }
        false
    }

    fn change(&mut self, _props: Self::Properties) -> bool {
        false
    }

    fn view(&self) -> Html {
        html! {
            <div id=self.config.id.clone() style=format!("height:{}", &self.props.height)>
                { self.content.clone() }
            </div>
        }
    }
}

