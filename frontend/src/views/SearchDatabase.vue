<template>
  <v-container>
    <v-card elevation="0">
      <v-card-title>
        Search Database
      </v-card-title>
      <v-img src="/img/search_capture1.png" alt="Search section" />
    </v-card>

    <v-container>
      <v-row justify="center">
        <v-btn class="ma-2">Download raw files</v-btn>
        <v-btn class="ma-2" :to="{name: 'TestSelection'}">View tests results</v-btn>
        <v-btn class="ma-2" :to="{name: 'CCFatigueAnalysis'}">Analyse experiment</v-btn>
      </v-row>
    </v-container>
  </v-container>
</template>

<script>
export default {
  name: 'SearchDatabase',
}
</script>

<style scoped lang="scss">
</style>
