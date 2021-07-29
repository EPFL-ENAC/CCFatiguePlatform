<template>
  <div>
    <!-- <v-skeleton-loader v-if="!dataIsFetched"
      max-width="100%" height="10rem"
      type="paragraph" />

    <template v-else> -->
      <experiment-specifications
        :experience="test.experience"
      />
      <v-container>

        <v-card elevation="0">
          <v-card-title>
            Test selection
          </v-card-title>
          <v-img src="/img/test_selection_capture1.png" alt="Test selection section" />

          <v-container>
            <v-row justify="end">
              <v-btn class="ma-2" :to="{name: 'TestDashboard'}">View tests</v-btn>
            </v-row>
          </v-container>
        </v-card>
      </v-container>
    <!-- </template> -->
  </div>
</template>

<script>
import { mapState, mapGetters } from 'vuex'
import ExperimentSpecifications from '@/components/ExperimentSpecifications.vue'

export default {
  name: 'TestSelection',
  components: {
    ExperimentSpecifications,
  },
  computed: {
    ...mapState([
      'test',
    ]),
    ...mapGetters([
      'dataIsFetched',
    ]),
  },
  created() {
    // this.$store.dispatch('fetchExperience', {
    //   laboratory: 'CCLAB',
    //   researcher: 'Vahid',
    //   experienceType: 'FA',
    //   date: '2021-04-20',
    //   testNumbers: [2, 5],  // This doesn't make sense ! TODO
    // })
  },
}
</script>

<style scoped lang="scss">
</style>
