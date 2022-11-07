<template>
  <v-container fluid>
    <p>- not implemented yet -</p>
    <v-row>
      <v-col cols="6">
        <cycle-counting></cycle-counting>
      </v-col>
      <v-col cols="6">
        <sn-curve></sn-curve>
      </v-col>
      <v-col cols="6">
        <cld-analysis></cld-analysis>
      </v-col>
      <v-col cols="6">
        <fatigue-failure></fatigue-failure>
      </v-col>
      <v-col cols="6">
        <damage-summation></damage-summation>
      </v-col>
    </v-row>
  </v-container>
</template>

<script>
import CldAnalysis from "@/components/analysis/CldAnalysis.vue";
import CycleCounting from "@/components/analysis/CycleCounting.vue";
import DamageSummation from "@/components/analysis/DamageSummation.vue";
import FatigueFailure from "@/components/analysis/FatigueFailure.vue";
import SnCurve from "@/components/analysis/SnCurve.vue";

export default {
  name: "CCFatigueAnalysis",
  components: {
    CldAnalysis,
    CycleCounting,
    DamageSummation,
    FatigueFailure,
    SnCurve,
  },
};
</script>
