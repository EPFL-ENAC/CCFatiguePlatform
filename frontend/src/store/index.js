import Vue from "vue";
import Vuex from "vuex";

import ApiClient from "@/backend/src/ApiClient";
import {
  AnalysisApi,
  DefaultApi,
  ExperimentsApi,
  TestsApi,
} from "@/backend/src/index";
import experiments from "./experiments";

Vue.use(Vuex);

const apiClient = new ApiClient(process.env.VUE_APP_API_URL);
Vue.prototype.$experimentsApi = new ExperimentsApi(apiClient);
Vue.prototype.$testsApi = new TestsApi(apiClient);
Vue.prototype.$defaultApi = new DefaultApi(apiClient);
Vue.prototype.$analysisApi = new AnalysisApi(apiClient);

export default new Vuex.Store({
  modules: {
    experiments,
  },
});
