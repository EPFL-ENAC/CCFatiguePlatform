<template>
  <div id="app">
    <TabMenu :model="routerMenu" />
    <router-view/>
  </div>
</template>

<script>
import TabMenu from 'primevue/tabmenu';

export default {
  data () {
    return {
      routerMenu: [
        {label: 'Home', icon: 'pi pi-fw pi-home', to: '/home'},
        {label: 'Database', icon: 'pi pi-fw pi-table', to: '/database'},
        {label: 'Data upload ', icon: 'pi pi-fw pi-send', to: '/dataupload'},
        {label: 'About', icon: 'pi pi-fw pi-info-circle', to: '/about'},
      ]
    }
  },
  components: {
    TabMenu,
  },
}
</script>


<style lang="scss">
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
}
</style>
