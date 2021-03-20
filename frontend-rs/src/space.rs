use yew::Properties;
use yew::prelude::*;

pub enum SpaceMsg {

}

pub struct Space {
    pub link: ComponentLink<Self>,
}

#[derive(Clone, Properties)]
pub struct Props {
}

fn grid() -> Html {
    // TODO - make this work
    /*
    (0..1000).step_by(50).into_iter().map(move |x| {
        (0..1000).step_by(50).into_iter().map(move |y| {
            html!{
                <circle id="orange-circle" r="5" cx={x} cy={y} fill="orange" />
            }
        })
    })
    */
    html! { <circle id="orange-circle" r="5" cx="5" cy="5" fill="orange" />}
}

impl Component for Space {
    type Message = SpaceMsg;
    type Properties = Props;

    fn create(props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
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
            <svg height="1000" width="1000">
            {
                grid()
            }
            </svg>
            </div>
        }
    }

}

