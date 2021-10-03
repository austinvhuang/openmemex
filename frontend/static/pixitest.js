const view = document.querySelector('.view')
let width, height, app

function initDimensions() {
  width = window.innerWidth
  height = window.innerHeight*10
}

function initApp() {
  app = new PIXI.Application({ view })
  app.renderer.autoDensity = true
  app.renderer.resize(width, height)
}

function initBackground() {
  background = new PIXI.Sprite()
  background.width = width
  background.height = height
  //const backgroundFragmentShader = resources['pixitest.glsl'].data
  //const backgroundFilter = new PIXI.Filter(undefined, backgroundFragmentShader)
  //background.filters[backgroundFilter]
  //app.stage.addChild(background)
}

function init() {
  initDimensions()
  initApp()
  initBackground()
  for (let i=0; i < 10; i++) {
    // TODO - catch file missing exception
    let testImage = PIXI.Sprite.from('./screenshots/' + ('' + i).padStart(10, '0') + '.png')
//    let scale= 0.08;
//    let offset = 2;
//    let ncol = 28;
    let scale= 0.5;
    let offset = 2;
    let ncol = 4;
    testImage.scale.set(scale, scale);
    testImage.anchor.x = 0
    testImage.anchor.y = 0
    testImage.position.x = (600 * scale + offset) * (i % ncol)
    testImage.position.y = (800 * scale + offset) * Math.floor(i / ncol)
  app.stage.addChild(testImage)
  }
}

const resources = PIXI.Loader.shared.resources
PIXI.Loader.shared.add(['pixitest.glsl']).load(init)

// init()
