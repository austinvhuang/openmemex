use yew::prelude::*;
use yew_router::prelude::*;
use yew_router::*;

#[derive(Switch, Debug, Clone)]
pub enum AppRoute {
    /*
    #[to = "/cards"]
    Cards,
    #[to = "/timeline"]
    Timeline,
    */
    #[to = "/addnote"]
    AddNote,
    #[to = "/"]
    Home,
}
