<template>
  <div class="ma-5">
    <v-skeleton-loader v-if="!dataIsFetched"
      max-width="100%" height="10rem"
      type="paragraph" />

    <template v-else>
      <v-card elevation="0">
        <v-card-title>
          Experiment : {{ experience['Experiment Type'] }} test, by {{ experience.Researcher }}, {{ experience.Laboratory }}, {{ experience.Experiment.Date }}
        </v-card-title>
      </v-card>

      <experiment-specifications
        :experience="experience"
      />
    </template>
  </div>
</template>

<script>
import { mapState, mapGetters } from 'vuex'
import ExperimentSpecifications from '@/components/ExperimentSpecifications.vue'

export default {
  name: 'CCFatigueAnalysis',
  components: {
    ExperimentSpecifications,
  },
  computed: {
    ...mapState([
      'experience',
    ]),
    ...mapGetters([
      'dataIsFetched',
    ]),
  },
  created() {
    this.$store.dispatch('fetchExperience')
  },
}
</script>

<style scoped>
</style>
