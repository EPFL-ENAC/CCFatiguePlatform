import Vue from "vue";
import VueRouter from "vue-router";
import Home from "../views/Home.vue";
// route level code-splitting
// this generates a separate chunk (name.[hash].js) for each route
// which is lazy-loaded when the route is visited.
const SearchDatabase = () =>
  import(/* webpackChunkName: "search" */ "../views/SearchDatabase.vue");
const TestsSelection = () =>
  import(
    /* webpackChunkName: "tests_selection" */ "../views/TestsSelection.vue"
  );
const TestDashboard = () =>
  import(/* webpackChunkName: "test_dashboard" */ "../views/TestDashboard.vue");
const DataUpload = () =>
  import(/* webpackChunkName: "data_upload" */ "../views/DataUpload.vue");
const CCFatigueAnalysis = () =>
  import(
    /* webpackChunkName: "ccfatigue_analysis" */ "../views/CCFatigueAnalysis.vue"
  );
const About = () =>
  import(/* webpackChunkName: "about" */ "../views/About.vue");

Vue.use(VueRouter);

const routes = [
  {
    path: "/",
    name: "Root",
    redirect: { name: "Home" },
  },
  {
    path: "/home",
    name: "Home",
    component: Home,
  },
  {
    path: "/fatigue_database",
    name: "FatigueDatabase",
    redirect: { name: "SearchDatabase" },
  },
  {
    path: "/fatigue_database/search",
    name: "SearchDatabase",
    component: SearchDatabase,
  },
  {
    path: "/fatigue_database/tests_selection/:experimentId",
    name: "TestsSelection",
    component: TestsSelection,
    props(route) {
      // typecast experimentId to Number
      const props = { ...route.params };
      props.experimentId = +props.experimentId;
      return props;
    },
  },
  {
    path: "/fatigue_database/test_dashboard",
    name: "TestDashboard",
    component: TestDashboard,
  },
  {
    path: "/fatigue_database/data_upload",
    name: "DataUpload",
    component: DataUpload,
  },
  {
    path: "/ccfatigue_analysis",
    name: "CCFatigueAnalysis",
    component: CCFatigueAnalysis,
  },
  {
    path: "/about",
    name: "About",
    component: About,
  },
];

const router = new VueRouter({
  mode: "history",
  base: process.env.BASE_URL,
  routes,
});

export default router;
