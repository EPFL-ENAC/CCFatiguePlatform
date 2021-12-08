export default {
  namespaced: true,
  state: {
    loading: false,
    experiments: [],
    nbExperiments: 0,
    page: 1,
    itemsPerPage: 10,
  },
  mutations: {
    nowWeLoad(state) {
      state.loading = true;
    },
    storePage(state, page) {
      state.page = page;
    },
    storeItemsPerPage(state, itemsPerPage) {
      state.itemsPerPage = itemsPerPage;
    },
    storeExperiments(state, data) {
      state.experiments = data.items;
      state.nbExperiments = data.total;
      state.loading = false;
    },
  },
  actions: {
    fetch({ commit, state }, options = {}) {
      commit("nowWeLoad");
      if (options.page && options.page != state.page) {
        commit("storePage", options.page);
      }
      if (options.itemsPerPage && options.itemsPerPage != state.itemsPerPage) {
        commit("storeItemsPerPage", options.itemsPerPage);
      }
      let opts = {
        page: state.page,
        size: state.itemsPerPage,
      };
      this._vm.$experimentsApi.getExperimentsExperimentsGet(
        opts,
        (error, data) => {
          if (error) {
            console.error(error);
          } else {
            commit("storeExperiments", data);
          }
        }
      );
    },
  },
};
