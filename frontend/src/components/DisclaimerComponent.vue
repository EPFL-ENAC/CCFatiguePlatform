<template>
  <v-snackbar v-model="snackbarVisible" :timeout="timeout" :color="color">
    <v-card :color="color" flat>
      <v-card-title>
        <v-icon class="pr-3" large> {{ icon }} </v-icon> {{ title }}
      </v-card-title>
      <v-card-text>
        {{ details }}
      </v-card-text>
    </v-card>
  </v-snackbar>
</template>

<script>
export default {
  name: "DisclaimerComponent",
  props: {
    title: { type: String, default: "" },
    details: { type: String, default: "" },
    icon: { type: String, default: "mdi-information-outline" },
    color: { type: String, default: "grey darken-4" },
    timeout: { type: Number, default: 4000 },
  },
  data() {
    return {
      snackbarVisible: true,
    };
  },
};
</script>

<style scoped lang="scss"></style>
