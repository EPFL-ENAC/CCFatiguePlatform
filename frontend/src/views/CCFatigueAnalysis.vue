<template>
  <v-container fluid class="pa-4 analysis-page">
    <v-row no-gutters class="fill-height">
      <!-- Left menu -->
      <v-col cols="12" md="3" lg="2" class="pr-md-4 mb-4 mb-md-0">
        <v-card class="menu-card pa-2" elevation="3" rounded="lg">
          <v-card-title class="text-h6 font-weight-bold pb-2">
            Main Menu
          </v-card-title>

          <v-divider class="mb-3" />

          <v-card-text class="pa-0 d-flex flex-column">
            <v-btn
              v-for="module in modules"
              :key="module.key"
              block
              height="52"
              class="mb-3 text-none justify-start menu-btn px-4"
              :color="selectedModule === module.key ? 'primary' : undefined"
              :variant="selectedModule === module.key ? 'flat' : 'outlined'"
              @click="selectedModule = module.key"
            >
              {{ module.label }}
            </v-btn>
          </v-card-text>
        </v-card>
      </v-col>

      <!-- Right content -->
      <v-col cols="12" md="9" lg="10">
        <v-card class="content-card pa-3" elevation="3" rounded="lg">
          <v-card-text class="pa-0 content-wrapper">
            <keep-alive>
              <component
                :is="currentComponent"
                @go-to-module="selectedModule = $event"
              />
            </keep-alive>
          </v-card-text>
        </v-card>
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
  data() {
    return {
      selectedModule: "cycle-counting",
      modules: [
        {
          key: "cycle-counting",
          label: "Cycle Counting",
          component: CycleCounting,
        },
        {
          key: "sn-curve",
          label: "S-N Curve",
          component: SnCurve,
        },
        {
          key: "cld-analysis",
          label: "CLD",
          component: CldAnalysis,
        },
        {
          key: "fatigue-failure",
          label: "Fatigue Failure",
          component: FatigueFailure,
        },
        {
          key: "damage-summation",
          label: "Damage Summation",
          component: DamageSummation,
        },
      ],
    };
  },
  computed: {
    currentModuleObject() {
      return (
        this.modules.find((module) => module.key === this.selectedModule) ||
        this.modules[0]
      );
    },
    currentComponent() {
      return this.currentModuleObject.component;
    },
  },
};
</script>

<style scoped>
.analysis-page {
  min-height: calc(100vh - 80px);
  background-color: #f7f8fa;
}

.menu-card {
  height: 100%;
  min-height: calc(100vh - 110px);
}

.content-card {
  min-height: calc(100vh - 110px);
}

.content-wrapper {
  width: 100%;
}

.menu-btn {
  font-size: 0.98rem;
  font-weight: 600;
  letter-spacing: 0.2px;
  border-radius: 10px;
}
</style>
