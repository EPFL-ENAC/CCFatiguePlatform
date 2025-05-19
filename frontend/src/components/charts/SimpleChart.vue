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
import { formatNumber3 } from "@/utils/formatters";
import { colorPalette } from "@/utils/style";
import { LineChart, ScatterChart } from "echarts/charts";
import {
  DataZoomComponent,
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
  DataZoomComponent,
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
    yAxisName: { type: String, default: "" },
    xAxisType: { type: String, default: "value" },
    dataZoom: { type: String, default: "" },
    color: { type: Array, default: () => colorPalette },
    xAxisMin: { type: [Number, null], default: null },
    xAxisMax: { type: [Number, null], default: null },
    axisLabelFormatter: {
      type: Function,
      default: formatNumber3,
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
    actualOption() {
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
        /*
        xAxis: {
          type: this.xAxisType,
          name: this.xAxisName,
          nameLocation: "middle",
          nameGap: 26,
          min: "dataMin",
          max: "dataMax",
          axisLabel: {
            formatter: formatNumber,
            hideOverlap: true,
          },
        },
        */
        xAxis: {
          type: this.xAxisType,
          name: this.xAxisName,
          nameLocation: "middle",
          nameGap: 26,
          min: this.xAxisMin != null ? this.xAxisMin : "dataMin",
          max: this.xAxisMax != null ? this.xAxisMax : "dataMax",
          axisLabel: {
            formatter: this.axisLabelFormatter,
            hideOverlap: true,
          },
        },
        yAxis: {
          name: this.yAxisName,
          nameLocation: "middle",
          nameGap: 50,
          /* min: "dataMin", */
          min: 0,
          max: "dataMax",
          axisLabel: {
            formatter: this.axisLabelFormatter,
            hideOverlap: true,
          },
        },
        tooltip: {
          trigger: "axis",
          confine: true,
          formatter: this.axisLabelFormatter,
        },
        dataZoom: [
          {
            id: "dataZoomX",
            xAxisIndex: [0],
            filterMode: "none",
            type: "inside",
          },
          {
            id: "dataZoomY",
            yAxisIndex: [0],
            filterMode: "none",
            type: "inside",
          },
        ],
        series: this.series.map((serie) => merge(serie, { showSymbol: false })),
        color: this.color,
      };
    },
  },
};
</script>
