use yew::prelude::*;
use yew::Properties;

pub enum SettingsMsg {}

pub enum FieldType {
    IntField,
}

pub struct IntFieldData {
    pub min: i32,
    pub max: i32,
}

pub struct Setting {
    pub field: String,
    pub field_type: FieldType,
}

pub struct Settings {
    pub link: ComponentLink<Self>,
}

#[derive(Clone, Properties)]
pub struct Props {}

    /*
impl SettingComponent for IntFieldData {
    fn render(&mut self) -> {

    }
}
    */
impl Component for Settings {
    type Message = SettingsMsg;
    type Properties = Props;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self { link: link }
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
            {"Settings"}
            </div>
        }
    }
}
