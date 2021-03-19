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
pub extern "C" {
    pub fn init_ace() -> JsValue;
}
