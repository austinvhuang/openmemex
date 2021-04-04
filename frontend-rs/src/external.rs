use wasm_bindgen::prelude::*;
use yew::web_sys::console;

/*
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen]
    fn uuidv4() -> JsValue;
}
*/

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen]
    pub fn init_ace() -> JsValue;

    #[wasm_bindgen]
    pub fn ace_add_callback(editor: JsValue, callback: &dyn Fn(JsValue) -> ()) -> JsValue;
}
