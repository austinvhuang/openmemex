use yew_router::prelude::*;

#[derive(Switch, Debug, Clone)]
pub enum AppRoute {
    /*
    #[to = "/cards"]
    Cards,
    #[to = "/timeline"]
    Timeline,
    */
    #[to = "/frontend/addnote"]
    AddNote,
    #[to = "/frontend/detail"]
    Detail,
    #[to = "/frontend/index.html"]
    Gallery,
}
