import {
  AnalysisApi,
  ApiClient,
  DefaultApi,
  ExperimentsApi,
  TestsApi,
} from "@/backend/index";
import numeral from "numeral";
import Vue from "vue";
import numFormat from "vue-filter-number-format";
import App from "./App.vue";
import "./plugins/pluralize";
import vuetify from "./plugins/vuetify";
import router from "./router";
import store from "./store";

Vue.config.productionTip = false;

numeral.register("locale", "fr-CH", {
  delimiters: {
    thousands: "'",
    decimal: ",",
  },
  abbreviations: {
    thousand: "k",
    million: "m",
    billion: "b",
    trillion: "t",
  },
  ordinal: function (number) {
    return number === 1 ? "er" : "ème";
  },
  currency: {
    symbol: "CHF",
  },
});

// switch between locales
numeral.locale("fr-CH");

Vue.filter("numFormat", numFormat(numeral));

const apiClient = new ApiClient(process.env.VUE_APP_API_URL);
Vue.prototype.$experimentsApi = new ExperimentsApi(apiClient);
Vue.prototype.$testsApi = new TestsApi(apiClient);
Vue.prototype.$defaultApi = new DefaultApi(apiClient);
Vue.prototype.$analysisApi = new AnalysisApi(apiClient);

new Vue({
  router,
  store,
  vuetify,
  render: (h) => h(App),
}).$mount("#app");
