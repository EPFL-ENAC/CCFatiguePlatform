<template>
  <v-app>
    <v-app-bar app color="#ff0000" dense dark>
      <v-tabs align-with-title fixed-tabs>
        <v-tab
          v-for="(route, i) in routerMenu" :key="i"
          :to="route.to"
          exact
        ><v-icon>{{ route.icon }}</v-icon>&nbsp;{{ route.label}} </v-tab>
      </v-tabs>
    </v-app-bar>

    <v-main>
      <router-view/>
    </v-main>
  </v-app>
</template>

<script>
export default {
  name: 'App',

  data () {
    return {
      routerMenu: [
        {label: 'Home', icon: 'mdi-home', to: '/home'},
        {label: 'Test dashboard', icon: 'mdi-chart-line', to: '/test-dashboard'},
        {label: 'Data upload ', icon: 'mdi-upload', to: '/data-upload'},
        {label: 'About', icon: 'mdi-information', to: '/about'},
      ]
    }
  },
}
</script>

<style scoped lang="scss">
</style>
