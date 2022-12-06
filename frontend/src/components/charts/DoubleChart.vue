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
import { formatNumber } from "@/utils/formatters";
import { colorPalette } from "@/utils/style";
import { LineChart, ScatterChart } from "echarts/charts";
import {
  GridComponent,
  LegendComponent,
  TitleComponent,
  TooltipComponent,
} from "echarts/components";
import { use } from "echarts/core";
import { CanvasRenderer } from "echarts/renderers";
import { merge } from "lodash";
import VChart from "vue-echarts";

use([
  CanvasRenderer,
  LineChart,
  ScatterChart,
  GridComponent,
  LegendComponent,
  TitleComponent,
  TooltipComponent,
]);

export default {
  name: "SimpleChart",
  components: {
    VChart,
  },
  props: {
    aspectRatio: { type: Number, default: 1 },
    series: { type: Array, default: () => [] },
    title: { type: String, default: "" },
    xAxisName: { type: String, default: "" },
    y1AxisName: { type: String, default: "" },
    y2AxisName: { type: String, default: "" },
    xAxisType: { type: String, default: "" },
    color: { type: Array, default: () => colorPalette },
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
          type: this.xAxisType,
          name: this.xAxisName,
          nameLocation: "middle",
          nameGap: 26,
          min: "dataMin",
          max: "dataMax",
          axisLabel: {
            formatter: formatNumber,
            fontSize: 10,
          },
        },
        yAxis: [
          {
            name: this.y1AxisName,
            nameLocation: "middle",
            nameGap: 30,
            min: "dataMin",
            max: "dataMax",
            axisLabel: {
              formatter: formatNumber,
              fontSize: 10,
            },
          },
          {
            name: this.y2AxisName,
            nameLocation: "middle",
            nameGap: 30,
            min: "dataMin",
            max: "dataMax",
            axisLabel: {
              formatter: formatNumber,
              fontSize: 10,
            },
          },
        ],
        tooltip: {
          trigger: "axis",
          confine: true,
          valueFormatter: formatNumber,
        },
        series: this.series.map((serie) => merge(serie, { showSymbol: false })),
        color: this.color,
      };
    },
  },
};
</script>
