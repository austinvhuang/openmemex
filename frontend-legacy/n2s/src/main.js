import Vue from 'vue'
import App from './App.vue'
import VTooltip from 'v-tooltip'
import vuetify from './plugins/vuetify';
// import embed from 'vega-embed'

// Vue.config.productionTip = false

Vue.use(VTooltip)

new Vue({
  vuetify,
  render: h => h(App)
}).$mount('#app')
