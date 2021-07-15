import Vue from 'vue'
import Axios from 'axios'
import App from './App.vue'
import router from './router'
import store from './store'
import vuetify from './plugins/vuetify'

Vue.config.productionTip = false

Axios.defaults.baseURL = 'http://localhost:8000/'
Vue.prototype.$http = Axios

new Vue({
  router,
  store,
  vuetify,
  render: h => h(App)
}).$mount('#app')
