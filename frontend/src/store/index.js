import Vue from 'vue'
import Vuex from 'vuex'
import Axios from 'axios'

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
    fetchExperience(context, payload) {
      const params = {
        laboratory: payload.laboratory,
        researcher: payload.researcher,
        experienceType: payload.experienceType,
        date: payload.date,
        testNumber: payload.testNumber,
      }
      Axios.get('experience/test', { params: params })
      .then((res) => {
        context.commit('saveTest', res.data)
      })
      .catch(err => console.log(err))
    }
  },
  modules: {
  }
})
