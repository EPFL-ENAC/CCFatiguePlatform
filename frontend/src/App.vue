<template>
  <v-app>
    <v-app-bar app color="primary" dense dark>
      <v-tabs align-with-title fixed-tabs>
        <v-tab
          v-for="(route, i) in routerMenu" :key="i"
          :to="route.to"
          exact
        ><v-icon>{{ route.icon }}</v-icon><template v-if="!route.iconOnly">&nbsp;{{ route.label}} </template></v-tab>
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
        {label: 'Home', icon: 'mdi-home', to: '/home', iconOnly: true},
        {label: 'Test dashboard', icon: 'mdi-chart-line', to: '/test-dashboard', iconOnly: false},
        {label: 'Data upload ', icon: 'mdi-upload', to: '/data-upload', iconOnly: false},
        {label: 'About', icon: 'mdi-information', to: '/about', iconOnly: true},
      ]
    }
  },
}
</script>

<style scoped lang="scss">
</style>
