export default {
  namespaced: true,
  state: {
    experiments: [],
    totalCount: 0,
  },
  mutations: {
    storeExperiments(state, data) {
      state.experiments = data;
      state.totalCount = data.length;
    },
  },
  actions: {
    fetchAll(context) {
      this._vm.$experimentsApi.getExperimentsExperimentsGet((error, data) => {
        if (error) {
          console.error(error);
        } else {
          context.commit("storeExperiments", data);
        }
      });
    },
  },
};
