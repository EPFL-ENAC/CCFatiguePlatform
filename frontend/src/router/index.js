import Vue from 'vue'
import VueRouter from 'vue-router'
import Home from '../views/Home.vue'
// route level code-splitting
// this generates a separate chunk (name.[hash].js) for each route
// which is lazy-loaded when the route is visited.
const SearchDatabase = () => import(/* webpackChunkName: "search" */ '../views/SearchDatabase.vue')
const TestSelection = () => import(/* webpackChunkName: "test-selection" */ '../views/TestSelection.vue')
const TestDashboard = () => import(/* webpackChunkName: "test-dashboard" */ '../views/TestDashboard.vue')
const DataUpload = () => import(/* webpackChunkName: "data-upload" */ '../views/DataUpload.vue')
const CCFatigueAnalysis = () => import(/* webpackChunkName: "ccfatigue-analysis" */ '../views/CCFatigueAnalysis.vue')
const About = () => import(/* webpackChunkName: "about" */ '../views/About.vue')

Vue.use(VueRouter)

const routes = [
  {
    path: '/',
    name: 'Root',
    redirect: {name: 'Home'},
  },
  {
    path: '/home',
    name: 'Home',
    component: Home,
  },
  {
    path: '/fatigue_database',
    name: 'FatigueDatabase',
    redirect: {name: 'SearchDatabase'},
  },
  {
    path: '/fatigue_database/search',
    name: 'SearchDatabase',
    component: SearchDatabase,
  },
  {
    path: '/fatigue_database/test-selection',
    name: 'TestSelection',
    component: TestSelection,
  },
  {
    path: '/fatigue_database/test-dashboard',
    name: 'TestDashboard',
    component: TestDashboard,
  },
  {
    path: '/fatigue_database/data-upload',
    name: 'DataUpload',
    component: DataUpload,
  },
  {
    path: '/ccfatigue-analysis',
    name: 'CCFatigueAnalysis',
    component: CCFatigueAnalysis,
  },
  {
    path: '/about',
    name: 'About',
    component: About,
  },
]

const router = new VueRouter({
  mode: 'history',
  base: process.env.BASE_URL,
  routes
})

export default router
