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
    storeBokehPlots(state, data) {
      state.tests = data.tests;
      state.plots = data.plots;
      state.loading = false;
    },
  },
  actions: {
    fetchBokehPlots({ commit }, payload) {
      commit("nowWeLoad", payload);
      this._vm.$experimentsApi.getTestsDashboardPlotsExperimentsTestsDashboardPlotsGet(
        {
          experimentId: payload.experimentId,
          testIds: payload.testIds,
        },
        (error, data) => {
          if (error) {
            console.error(error);
          } else {
            commit("storeBokehPlots", data);
          }
        }
      );
    },
  },
};
