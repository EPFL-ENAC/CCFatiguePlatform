<template>
  <span>
    <span class="subject">{{ subject }}:</span> &nbsp;

    <span v-for="(value, valueIndex) in values" :key="valueIndex">
      <span :style="getStyle(valueIndex)">
        <span v-if="valueType === 'string'" class="value">
          {{ value }}
          <template v-if="!isNaN(value) && unit !== ''"> {{ unit }} </template>
        </span>

        <span v-else-if="valueType === 'bigNumber'" class="value">
          {{ value | numFormat("0,0") }}
          <template v-if="!isNaN(value) && unit !== ''"> {{ unit }} </template>
        </span>

        <span
          v-for="(pub, index) in value"
          v-else-if="valueType === 'PublicationsLinks'"
          :key="index"
        >
          <a :href="pub.DOI" target="_blank">{{ pub.Title }}</a>
          <span v-if="index != value.length - 1">, </span>
        </span>
      </span>
      <span v-if="valueIndex != values.length - 1">, </span>
    </span>
    <info-tooltip v-if="tooltip">{{ tooltip }}</info-tooltip>
  </span>
</template>

<script>
import InfoTooltip from "./InfoTooltip.vue";

export default {
  name: "ExperimentSV",
  components: { InfoTooltip },
  props: {
    subject: { type: String, default: "" },
    values: { type: Array, default: () => [] },
    valueType: {
      type: String,
      default: "string",
    },
    unit: {
      type: String,
      default: "",
    },
    colors: { type: Array, default: () => [] },
    tooltip: { type: String, default: "" },
  },
  methods: {
    getStyle(i) {
      if (this.colors) {
        return {
          color: this.colors[i],
        };
      } else {
        return {};
      }
    },
  },
};
</script>

<style scoped lang="scss">
span.subject {
  font-weight: bold;
  font-style: italic;
  color: rgb(143, 143, 143);
}
</style>
