export default {
  namespaced: true,
  state: {
    loading: false,
    experiments: [],
    nbExperiments: 0,
    page: 1,
    itemsPerPage: 10,
    allFractureMode: [],
    allMaterialTypeFiberMaterial: [],
    allMaterialTypeResin: [],
    allLaminatesAndAssembliesStackingSequence: [],
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
    emptyExperiments(state) {
      state.experiments = [];
      state.nbExperiments = 0;
      state.loading = false;
    },
    storeAllFractureMode(state, data) {
      state.allFractureMode = data;
    },
    storeAllMaterialTypeFiberMaterial(state, data) {
      state.allMaterialTypeFiberMaterial = data;
    },
    storeAllMaterialTypeResin(state, data) {
      state.allMaterialTypeResin = data;
    },
    storeAllLaminatesAndAssembliesStackingSequence(state, data) {
      state.allLaminatesAndAssembliesStackingSequence = data;
    },
  },
  actions: {
    fetch({ commit, state }, payload) {
      if (payload.filterToEmpty) {
        commit("emptyExperiments");
        return;
      }
      commit("nowWeLoad");
      if (
        payload.paginationOptions.page &&
        payload.paginationOptions.page != state.page
      ) {
        commit("storePage", payload.paginationOptions.page);
      }
      if (
        payload.paginationOptions.itemsPerPage &&
        payload.paginationOptions.itemsPerPage != state.itemsPerPage
      ) {
        commit("storeItemsPerPage", payload.paginationOptions.itemsPerPage);
      }
      let opts = {
        page: state.page,
        size: state.itemsPerPage,
        query: payload.query,
        textSearch: payload.textSearch,
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
    fetchAllFiltersValues({ commit }) {
      this._vm.$experimentsApi.getFractureModeDistinctExperimentsFractureModeDistinctGet(
        (error, data) => {
          if (error) {
            console.error(error);
          } else {
            commit("storeAllFractureMode", data);
          }
        }
      );
      this._vm.$experimentsApi.getMaterialTypeFiberMaterialDistinctExperimentsMaterialTypeFiberMaterialDistinctGet(
        (error, data) => {
          if (error) {
            console.error(error);
          } else {
            commit("storeAllMaterialTypeFiberMaterial", data);
          }
        }
      );
      this._vm.$experimentsApi.getMaterialTypeResinDistinctExperimentsMaterialTypeResinDistinctGet(
        (error, data) => {
          if (error) {
            console.error(error);
          } else {
            commit("storeAllMaterialTypeResin", data);
          }
        }
      );
      this._vm.$experimentsApi.getLaminatesAndAssembliesStackingSequenceDistinctExperimentsLaminatesAndAssembliesStackingSequenceDistinctGet(
        (error, data) => {
          if (error) {
            console.error(error);
          } else {
            commit("storeAllLaminatesAndAssembliesStackingSequence", data);
          }
        }
      );
    },
  },
};
