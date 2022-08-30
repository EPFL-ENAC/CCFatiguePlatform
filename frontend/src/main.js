import Vue from "vue";
import App from "./App.vue";
import router from "./router";
import store from "./store";
import vuetify from "./plugins/vuetify";
import "./plugins/pluralize";
import numeral from "numeral";
import numFormat from "vue-filter-number-format";

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

new Vue({
  router,
  store,
  vuetify,
  render: (h) => h(App),
}).$mount("#app");
