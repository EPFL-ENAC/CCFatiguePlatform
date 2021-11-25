import Vue from 'vue'
import Vuex from 'vuex'
import Axios from 'axios'
import qs from 'qs'

Vue.use(Vuex)

export default new Vuex.Store({
  state: {
    test: {},
  },
  getters: {
    dataIsFetched(state) {
      return Object.keys(state.test).length > 0
    }
  },
  mutations: {
    saveTest(state, test) {
      state.test = { ...test }
    }
  },
  actions: {
    fetchExperiment(context, payload) {
      Axios.get('dashboard', {
        params: payload,
        paramsSerializer: params => qs.stringify(params, { arrayFormat: 'repeat' }),
      })
      .then((res) => {
        context.commit('saveTest', res.data)
      })
      .catch(err => console.log(err))
    }
  },
  modules: {
  }
})
