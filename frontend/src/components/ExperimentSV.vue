<template>
  <span>
    <span class="subject">{{ subject }}:</span> &nbsp;

    <span v-for="(value, valueIndex) in values" :key="valueIndex">
      <span :class="classIter(valueIndex)">
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
    }
  },
  methods: {
    classIter(i) {
      const classes = ['one', 'two', 'three']
      i = i % classes.length
      if (this.values.length < 2) {
        return ''
      }
      return classes[i]
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
.one {
  color: blue;
}
.two {
  color: red;
}
.three {
  color: green;
}
</style>
