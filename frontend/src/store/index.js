import Vue from 'vue'
import Vuex from 'vuex'
import Axios from 'axios'

Vue.use(Vuex)

export default new Vuex.Store({
  state: {
    experience: {},
  },
  getters: {
    dataIsFetched(state) {
      return ('Laboratory' in state.experience)
    }
  },
  mutations: {
    saveExperience(state, experience) {
      state.experience = { ...experience }
    }
  },
  actions: {
    fetchExperience(context) {
      Axios.get('experience')
      .then((res) => {
        context.commit('saveExperience', res.data)
      })
      .catch(err => console.log(err))
    }
  },
  modules: {
  }
})
