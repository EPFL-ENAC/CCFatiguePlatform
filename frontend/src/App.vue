<template>
  <v-app>
    <v-app-bar app color="primary" dense dark>
      <v-tabs align-with-title fixed-tabs>
        <v-tab :to="{ name: 'Home' }" exact>
          <v-icon>mdi-home</v-icon>
        </v-tab>

        <v-tab :to="{ name: 'FatigueDatabase' }">
          <v-menu offset-y open-on-hover>
            <template v-slot:activator="{ on }">
              <v-btn color="primary" v-on="on" elevation="0">
                Fatigue database
              </v-btn>
            </template>
            <v-list color="primary">
              <v-list-item>
                <v-list-item-title>
                  <v-btn
                    color="primary"
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
                    color="primary"
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

        <v-tab :to="{ name: 'CCFatigueAnalysis' }" exact>
          CCFatigue analysis
        </v-tab>

        <v-tab :to="{ name: 'About' }" exact>
          <v-icon>mdi-information</v-icon>
        </v-tab>
      </v-tabs>
    </v-app-bar>

    <v-main>
      <disclaimer
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
import Disclaimer from "@/components/Disclaimer.vue";

export default {
  name: "App",
  components: {
    Disclaimer,
  },
};
</script>

<style scoped lang="scss"></style>
