use yew::prelude::*;
use yew::Properties;

pub enum SpaceMsg {
    PointHover(MouseEvent, String),
}

pub struct Space {
    pub link: ComponentLink<Self>,
}

#[derive(Clone, Properties)]
pub struct Props {}

fn grid() -> Html {
    (0..1000)
        .step_by(20)
        .map(move |x| {
            (0..1000)
                .step_by(20)
                .map(move |y| {
                    let suffix = format!("{:?}_{:?}", x, y);
                    let id = format!("circle-{:?}", suffix);
                    let id_pound = format!("#circle-{:?}", suffix);
                    let x_id_anim = format!("x-circle-anim-{:?}", suffix);
                    let y_id_anim = format!("y-circle-anim-{:?}", suffix);
                    let x_max = x + 50;
                    let y_max = y + 50;
                    html! {
                        <>
                        <circle id={id.clone()} r="5" cx={x} cy={y} fill="orange"  opacity="0.3">
                        <title> {"test point"} </title>
                        </circle>

                        <text x={x} y={y} class="small">{"Hello"}</text>

                        <animate
                        href={id_pound.clone()}
                        attributeName="cx"
                        from={x}
                        to={x}
                        values={ format!("{:?}; {:?}; {:?}", x, x_max, x) }
                        keyTimes="0; 0.5; 1"
                        dur="2s"
                        begin="0s"
                        repeatCount="indefinite"
                        id={x_id_anim} />

                        <animate
                        href={id_pound.clone()}
                        attributeName="cy"
                        from={y}
                        to={y}
                        values={ format!("{:?}; {:?}; {:?}", y, y_max, y) }
                        keyTimes="0; 0.5; 1"
                        dur="2s"
                        begin="0s"
                        repeatCount="indefinite"
                        id={y_id_anim} />

                        </>
                    }
                })
                .collect::<Html>()
        })
        .collect::<Html>()
}

impl Component for Space {
    type Message = SpaceMsg;
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
                <div class="container shadow p-3 mb-5 bg-body rounded">
                    <iframe class="responsive-iframe shadow p-3 mb-5 bg-body rounded" sandbox="allow-same-origin allow-scripts allow-popups allow-forms"
                    src="contentspace.html" />
                </div>


            <svg height="1000" width="1000">
            {
                grid()
            }
            </svg>
            </div>
        }
    }
}
