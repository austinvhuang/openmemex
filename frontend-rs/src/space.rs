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
    let mut counter = 0;
    (0..1000).step_by(20).map(move |x| {
        (0..1000).step_by(20).map(move |y| {
            let id = format!("circle-{:?}", counter);
            counter = counter + 1;
            html! {
                <circle id={id} r="5" cx={x} cy={y} fill="orange" />

                /*
                <animate 
                href="#orange-circle"
                attributeName="cx"
                from="50"
                to="450" 
                dur="5s"
                begin="click"
                repeatCount="2"
                fill="freeze" 
                id="circ-anim" />
                */

            }
        }).collect::<Html>()
    }).collect::<Html>()
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

