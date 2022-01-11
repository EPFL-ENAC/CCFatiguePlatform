import Vue from "vue";
import Vuex from "vuex";

import { DefaultApi, ExperimentsApi, TestsApi } from "@/backend/src/index";
import ApiClient from "@/backend/src/ApiClient";
import experiments from "./experiments";
import bokehPlots from "./bokehPlots";

Vue.use(Vuex);

const apiClient = new ApiClient(process.env.VUE_APP_API_URL);
Vue.prototype.$experimentsApi = new ExperimentsApi(apiClient);
Vue.prototype.$testsApi = new TestsApi(apiClient);
Vue.prototype.$defaultApi = new DefaultApi(apiClient);

export default new Vuex.Store({
  modules: {
    experiments,
    bokehPlots,
  },
});
