<template>
  <v-dialog
    v-model="dialog"
    width="500"
  >
    <template v-slot:activator="{ on, attrs }">
      <v-btn
        icon
        v-bind="attrs"
        v-on="on"
      >
        <v-icon>mdi-information-outline</v-icon>
      </v-btn>
    </template>

    <v-card>
      <v-card-title>
        <slot name="title" />
      </v-card-title>

      <v-card-text>
        <slot />
      </v-card-text>

      <v-divider></v-divider>

      <v-card-actions>
        <v-spacer></v-spacer>
        <v-btn
          color="primary"
          text
          @click="dialog = false"
        >
          OK
        </v-btn>
      </v-card-actions>
    </v-card>
  </v-dialog>
</template>

<script>
export default {
  name: 'InfoButton',
  data () {
    return {
      dialog: false,
    }
  },
}
</script>

<style scoped lang="scss">

</style>
