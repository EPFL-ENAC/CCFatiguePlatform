<template>
  <v-tooltip bottom max-width="500px" open-on-focus>
    <template v-slot:activator="{ on, attrs }">
      <v-btn icon v-bind="attrs" v-on="on">
        <v-icon>mdi-information-outline</v-icon>
      </v-btn>
    </template>
    <span>
      <slot></slot>
    </span>
  </v-tooltip>
</template>

<script>
export default {
  name: "InfoTooltip",
};
</script>

<style scoped lang="scss">
</style>
