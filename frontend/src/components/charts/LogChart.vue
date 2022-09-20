<template>
  <v-responsive :aspect-ratio="aspectRatio">
    <v-chart
      autoresize
      :option="actualOption"
      :update-options="updateOptions"
    ></v-chart>
  </v-responsive>
</template>

<script>
import { use } from "echarts/core";
import { CanvasRenderer } from "echarts/renderers";
import {
  GridComponent,
  LegendComponent,
  TitleComponent,
  TooltipComponent,
} from "echarts/components";
import { LineChart } from "echarts/charts";
import VChart from "vue-echarts";
import { merge } from "lodash";

use([
  CanvasRenderer,
  LineChart,
  GridComponent,
  LegendComponent,
  TitleComponent,
  TooltipComponent,
]);

export default {
  name: "LogChart",
  components: {
    VChart,
  },
  props: {
    aspectRatio: {
      type: Number,
      default: 1,
    },
    series: {
      type: Array,
    },
    title: {
      type: String,
    },
    xAxisName: {
      type: String,
    },
    yAxisName: {
      type: String,
    },
  },
  data() {
    return {
      updateOptions: {
        notMerge: true,
      },
    };
  },
  computed: {
    actualOption: function () {
      return {
        title: {
          text: this.title,
        },
        legend: {
          type: "scroll",
        },
        grid: {
          left: 50,
          top: 40,
          right: 50,
          bottom: 20,
          containLabel: true,
        },
        xAxis: {
          type: "log",
          name: this.xAxisName,
          nameLocation: "middle",
          nameGap: 26,
          axisLabel: {
            fontSize: 10,
          },
        },
        yAxis: {
          name: this.yAxisName,
          nameLocation: "middle",
          nameGap: 50,
        },
        tooltip: {
          trigger: "axis",
          confine: true,
          valueFormatter: (value) => value.toFixed(2),
        },
        series: this.series.map((serie) => merge(serie, { showSymbol: false })),
      };
    },
  },
};
</script>
