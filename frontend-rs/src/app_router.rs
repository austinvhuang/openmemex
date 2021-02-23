use wasm_bindgen::prelude::*;
use yew::prelude::*;
use yew_router::prelude::*;

use yew_router::*;

struct AppRouter {}

#[derive(Switch)]
enum AppRoute {
    #[to = "/cards"]
    Cards,
    #[to = "/timeline"]
    Timeline,
    #[to = "/addnote"]
    AddNote,
    #[to = "/"]
    Home,
}

impl Component for Model {
    type Message=  ();
    type Properties = ();

    fn create(_: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {}
    }

    fn update(&mut self, _props: Self::Properties) -> ShouldRender {
        false
    }

    fn change(&self, _props: Self::Properties) -> ShouldRender {
        false
    }

    fn view(&self) -> Html {
        let  render_func = Router::render(|route: AppRoute| match route {
        });

        html! {
            <Router<AppRoute, ()> render=render_func/>
        }
    }

}

