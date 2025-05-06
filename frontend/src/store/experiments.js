export default {
  namespaced: true,
  state: {
    filteredExperiments: {
      filters: {},
      experiments: [],
      pagination: {
        page: 1,
        size: 0,
        total: 0,
      },
      loading: false,
    },
    allFractureMode: [],
    allMaterialTypeFiberMaterial: [],
    allMaterialTypeResin: [],
    allLaminatesAndAssembliesStackingSequence: [],
    oneExperiment: {
      experimentId: null,
      experiment: {},
      tests: [],
      pagination: {
        page: 1,
        size: 0,
        total: 0,
      },
      loading: false,
      loadingTests: false,
    },
    units: {},
  },
  mutations: {
    nowWeLoadFilteredExperiments(state, payload) {
      state.filteredExperiments = {
        filters: payload.filters,
        experiments: [],
        pagination: {
          page: payload.pagination.page,
          size: payload.pagination.size,
          total: 0,
        },
        loading: true,
      };
    },
    nowWeLoadOneExperiment(state, payload) {
      state.oneExperiment = {
        experimentId: payload.experimentId,
        experiment: {},
        tests: [],
        pagination: {
          page: payload.pagination.page,
          size: payload.pagination.size,
          total: 0,
        },
        loading: true,
        loadingTests: true,
      };
    },
    storeFilteredExperiments(state, data) {
      state.filteredExperiments = {
        ...state.filteredExperiments,
        experiments: data.items,
        pagination: {
          page: data.page,
          size: data.size,
          total: data.total,
        },
        loading: false,
      };
    },
    emptyFilteredExperiments(state) {
      state.filteredExperiments = {
        ...state.filteredExperiments,
        experiments: [],
        pagination: {
          page: 1,
          size: state.filteredExperiments.pagination.size,
          total: 0,
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
    storeOneExperiment(state, data) {
      state.oneExperiment = {
        ...state.oneExperiment,
        experiment: data.items[0],
        loading: false,
      };
    },
    storeOneExperimentTests(state, data) {
      state.oneExperiment = {
        ...state.oneExperiment,
        tests: data.items,
        pagination: {
          page: data.page,
          size: data.size,
          total: data.total,
        },
        loadingTests: false,
      };
    },
    storeExperimentMetadata(state, metadata) {
      state.oneExperiment.experiment = {
        ...state.oneExperiment.experiment,
        ...metadata,
      };
    },
    storeUnits(state, data) {
      state.units = data.reduce((acc, item) => {
        acc[item.subject] = item.unit;
        return acc;
      }, {});
    },
  },
  actions: {
    fetchUnits({ commit, state }) {
      if (Object.keys(state.units).length === 0) {
        this._vm.$defaultApi.getUnits().then(
          (data) => commit("storeUnits", data),
          (error) => console.error(error)
        );
      }
    },
    fetchOneExperimentWithTests({ commit, state }, payload) {
      commit("nowWeLoadOneExperiment", payload);

      // 1. Fetch basic experiment info
      this._vm.$experimentsApi
        .getExperiments({ query: `id:${payload.experimentId}` })
        .then(
          (data) => commit("storeOneExperiment", data),
          (error) => console.error(error)
        );

      // 2. Fetch tests
      this._vm.$testsApi
        .getTests(payload.experimentId, {
          page: state.oneExperiment.pagination.page,
          size: state.oneExperiment.pagination.size,
        })
        .then(
          (data) => {
            commit("storeOneExperimentTests", data);

            // 3. Fetch metadata from first test if available
            if (data.items.length > 0) {
              const firstTestId = data.items[0].id;
              this._vm.$experimentsApi
                .getQuasiStaticTest(payload.experimentId, firstTestId)
                .then((qsData) => {
                  if (qsData && qsData.experiment_metadata) {
                    commit(
                      "storeExperimentMetadata",
                      qsData.experiment_metadata
                    );
                  }
                });
            }
          },
          (error) => console.error(error)
        );
    },
    fetchFilteredExperiments({ commit, state }, payload) {
      commit("nowWeLoadFilteredExperiments", payload);

      const queryElements = [];

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

      if (payload.filters.fiberMaterial !== null) {
        queryElements.push(
          `material_type_fiber_material:${payload.filters.fiberMaterial}`
        );
      }

      if (payload.filters.resin !== null) {
        queryElements.push(`material_type_resin:${payload.filters.resin}`);
      }

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
      this._vm.$experimentsApi.getExperiments(opts).then(
        (data) => commit("storeFilteredExperiments", data),
        (error) => console.error(error)
      );
    },
    fetchAllFiltersValues({ commit }) {
      this._vm.$experimentsApi.getFieldDistinct("fracture_mode").then(
        (data) => commit("storeAllFractureMode", data),
        (error) => console.error(error)
      );
      this._vm.$experimentsApi
        .getFieldDistinct("material_type_fiber_material")
        .then(
          (data) => commit("storeAllMaterialTypeFiberMaterial", data),
          (error) => console.error(error)
        );
      this._vm.$experimentsApi.getFieldDistinct("material_type_resin").then(
        (data) => commit("storeAllMaterialTypeResin", data),
        (error) => console.error(error)
      );
      this._vm.$experimentsApi
        .getFieldDistinct("laminates_and_assemblies_stacking_sequence")
        .then(
          (data) =>
            commit("storeAllLaminatesAndAssembliesStackingSequence", data),
          (error) => console.error(error)
        );
    },
  },
};
