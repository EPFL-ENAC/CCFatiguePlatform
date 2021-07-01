import Vue from 'vue'
import VueRouter from 'vue-router'
import Home from '../views/Home.vue'
// route level code-splitting
// this generates a separate chunk (name.[hash].js) for each route
// which is lazy-loaded when the route is visited.
const TestDashboard = () => import(/* webpackChunkName: "test-dashboard" */ '../views/TestDashboard.vue')
const DataUpload = () => import(/* webpackChunkName: "data-upload" */ '../views/DataUpload.vue')
const About = () => import(/* webpackChunkName: "about" */ '../views/About.vue')

Vue.use(VueRouter)

const routes = [
  {
    path: '/',
    name: 'Root',
    redirect: '/home'
  },
  {
    path: '/home',
    name: 'Home',
    component: Home
  },
  {
    path: '/test-dashboard',
    name: 'TestDashboard',
    component: TestDashboard
  },
  {
    path: '/data-upload',
    name: 'DataUpload',
    component: DataUpload
  },
  {
    path: '/about',
    name: 'About',
    component: About
  },
]

const router = new VueRouter({
  mode: 'history',
  base: process.env.BASE_URL,
  routes
})

export default router
