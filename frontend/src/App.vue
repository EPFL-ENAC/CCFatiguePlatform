<template>
  <v-app>
    <v-app-bar app class="custom-app-bar text-white" dense color="secondary">
      <v-img
        src="/img/GR_MecLogo.png"
        max-height="60"
        max-width="200"
        contain
        class="ml-2 mr-6"
      ></v-img>
      <v-tabs align-with-title fixed-tabs>
        <v-tab :to="{ name: 'Home' }" exact class="text-white">
          <v-icon>mdi-home</v-icon>
        </v-tab>

        <v-tab :to="{ name: 'MaterialDatabase' }">
          <v-menu offset-y open-on-hover>
            <template #activator="{ on }">
              <v-btn
                class="text-white"
                color="secondary"
                elevation="0"
                v-on="on"
              >
                Material database
              </v-btn>
            </template>
            <v-list color="secondary">
              <v-list-item>
                <v-list-item-title>
                  <v-btn
                    color="secondary"
                    elevation="0"
                    :to="{ name: 'SearchDatabase' }"
                  >
                    Search database
                  </v-btn>
                </v-list-item-title>
              </v-list-item>
              <v-list-item>
                <v-list-item-title>
                  <v-btn
                    color="secondary"
                    elevation="0"
                    :to="{ name: 'DataUpload' }"
                  >
                    Data upload
                  </v-btn>
                </v-list-item-title>
              </v-list-item>
            </v-list>
          </v-menu>
        </v-tab>

        <v-tab :to="{ name: 'CCFatigueAnalysis' }" exact class="text-white">
          CCFatigue analysis
        </v-tab>

        <v-tab :to="{ name: 'About' }" exact class="text-white">
          <v-icon>mdi-information</v-icon>
        </v-tab>
      </v-tabs>
    </v-app-bar>

    <v-main>
      <disclaimer-component
        title="Under development"
        details="This platform is under active development.
          Go to the about tab if you want to report bugs and suggestions."
        :timeout="3000"
      />
      <router-view />
    </v-main>
  </v-app>
</template>

<script>
import DisclaimerComponent from "@/components/DisclaimerComponent.vue";

export default {
  name: "App",
  components: {
    DisclaimerComponent,
  },
};
</script>

<!-- <style scoped lang="scss"></style> -->

<style lang="scss">
.custom-app-bar,
.custom-app-bar * {
  color: white !important;
}
</style>
