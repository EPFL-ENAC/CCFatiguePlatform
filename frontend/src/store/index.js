import Vue from "vue";
import Vuex from "vuex";

import { ExperimentsApi } from "@/backend/src/index";
import ApiClient from "@/backend/src/ApiClient";
import experiments from "./experiments";

Vue.use(Vuex);

const apiClient = new ApiClient(process.env.VUE_APP_API_URL);
Vue.prototype.$experimentsApi = new ExperimentsApi(apiClient);

export default new Vuex.Store({
  modules: {
    experiments,
  },
});
