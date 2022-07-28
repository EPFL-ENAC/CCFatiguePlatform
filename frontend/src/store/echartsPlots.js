export default {
  namespaced: true,
  state: {
    loading: false,
    tests: [],
    plots: {},
  },
  mutations: {
    nowWeLoad(state) {
      state.loading = true;
    },
    storeEchartsPlots(state, data) {
      state.tests = data.tests;
      state.plots = data.plots;
      state.loading = false;
    },
  },
  actions: {
    fetchEchartsPlots({ commit }, payload) {
      commit("nowWeLoad", payload);
      this._vm.$experimentsApi.getDataTestsDashboardPlotsExperimentsDataTestsDashboardPlotsGet(
        {
          experimentId: payload.experimentId,
          testIds: payload.testIds,
        },
        (error, data) => {
          if (error) {
            console.error(error);
          } else {
            commit("storeEchartsPlots", data);
          }
        }
      );
    },
  },
};
