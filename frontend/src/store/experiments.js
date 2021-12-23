export default {
  namespaced: true,
  state: {
    filteredExperiments: {
      filters: {}, // filters in UI
      experiments: [], // the experiments fetched from API
      pagination: {
        page: 1, // page number of current fetched experiments. Overwrirten by UI
        size: 0, // max number of fetched experiments returned per request. Overwrirten by UI
        total: 0, // number of experiments in DB matching the filters. Given by backend
      },
      loading: false, // used to let the user know that data is loading
    },
    allFractureMode: [],
    allMaterialTypeFiberMaterial: [],
    allMaterialTypeResin: [],
    allLaminatesAndAssembliesStackingSequence: [],
  },
  mutations: {
    nowWeLoadFilteredExperiments(state, payload) {
      state.filteredExperiments = {
        filters: payload.filters,
        experiments: [],
        pagination: {
          total: 0,
          page: payload.pagination.page,
          size: payload.pagination.size,
        },
        loading: true,
      };
    },
    storeItemsPerPage(state, itemsPerPage) {
      state.itemsPerPage = itemsPerPage;
    },
    storeFilteredExperiments(state, data) {
      state.filteredExperiments = {
        ...state.filteredExperiments,
        experiments: data.items,
        pagination: {
          total: data.total,
          page: data.page,
          size: data.size,
        },
        loading: false,
      };
    },
    emptyFilteredExperiments(state) {
      state.filteredExperiments = {
        ...state.filteredExperiments,
        experiments: [],
        pagination: {
          total: 0,
          page: 1,
          size: state.filteredExperiments.pagination.size,
        },
        loading: false,
      };
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
    fetchFilteredExperiments({ commit, state }, payload) {
      commit("nowWeLoadFilteredExperiments", payload);

      const queryElements = [];

      // type (FA|QS)
      const types = [
        payload.filters.typeFA ? "FA" : null,
        payload.filters.typeQS ? "QS" : null,
      ]
        .filter((val) => val !== null)
        .join(",");
      if (types === "") {
        commit("emptyFilteredExperiments");
        return;
      }
      queryElements.push("experiment_type:" + types);

      // fracture (true|false)
      // fracture_mode (All modes|Mode I|Mode II|Mode III|Combined)
      if (payload.filters.withFracture && !payload.filters.withoutFracture) {
        queryElements.push("fracture:1");
        if (payload.filters.fractureMode !== null) {
          queryElements.push(`fracture_mode:${payload.filters.fractureMode}`);
        }
      } else if (
        !payload.filters.withFracture &&
        payload.filters.withoutFracture
      ) {
        queryElements.push("fracture:0");
      } else if (
        !payload.filters.withFracture &&
        !payload.filters.withoutFracture
      ) {
        commit("emptyFilteredExperiments");
        return;
      }

      // material_type_fiber_material
      if (payload.filters.fiberMaterial !== null) {
        queryElements.push(
          `material_type_fiber_material:${payload.filters.fiberMaterial}`
        );
      }

      // material_type_resin
      if (payload.filters.resin !== null) {
        queryElements.push(`material_type_resin:${payload.filters.resin}`);
      }

      // laminates_and_assemblies_stacking_sequence
      if (payload.filters.stackingSequence !== null) {
        queryElements.push(
          `laminates_and_assemblies_stacking_sequence:${payload.filters.stackingSequence}`
        );
      }

      const opts = {
        page: state.filteredExperiments.pagination.page,
        size: state.filteredExperiments.pagination.size,
        query: queryElements.join(";"),
        textSearch: payload.filters.textSearch,
      };
      this._vm.$experimentsApi.getExperimentsExperimentsGet(
        opts,
        (error, data) => {
          if (error) {
            console.error(error);
          } else {
            commit("storeFilteredExperiments", data);
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
