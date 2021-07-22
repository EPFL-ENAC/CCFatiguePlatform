<template>
  <span>
    <span class="subject">{{ subject }}:</span> &nbsp;

    <span v-for="(value, valueIndex) in values" :key="valueIndex">
      <span :style="getStyle(valueIndex)">
        <span v-if="valueType==='string'" class="value">
          {{ value }}<template v-if="! isNaN(value) && unit !== ''" >{{ unit }}</template>
        </span>

        <span v-else-if="valueType==='bigNumber'" class="value">
          {{ value | numFormat('0,0') }}<template v-if="! isNaN(value) && unit !== ''" >{{ unit }}</template>
        </span>

        <span v-else-if="valueType==='PublicationsLinks'" v-for="(pub, index) in value" :key="index">
          <a :href="pub.DOI" target="_blank">{{ pub.Title }}</a>
          <span v-if="index != value.length - 1">, </span>
        </span>
      </span>
      <span v-if="valueIndex != values.length - 1">, </span>
    </span>
  </span>
</template>

<script>
export default {
  name: 'ExperimentSV',
  props: {
    subject: String,
    values: Array,
    valueType: {
      type: String,
      default: 'string',
    },
    unit: {
      type: String,
      default: '',
    },
    colors: Array,
  },
  methods: {
    getStyle(i) {
      if (this.colors) {
        return {
          color: this.colors[i]
        }
      } else {
        return {}
      }
    }
  }
}
</script>

<style scoped lang="scss">
span.subject {
  font-weight: bold;
  font-style: italic;
  color: rgb(143, 143, 143);
}
</style>
