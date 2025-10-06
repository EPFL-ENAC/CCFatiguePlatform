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
import { format3 } from "@/utils/formatters";
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
  name: "DoubleChart",
  components: { VChart },
  props: {
    aspectRatio: { type: Number, default: 1 },
    series: { type: Array, default: () => [] },
    title: { type: String, default: "" },
    xAxisName: { type: String, default: "" },
    y1AxisName: { type: String, default: "" },
    y2AxisName: { type: String, default: "" },
    xAxisType: { type: String, default: "value" },

    /* opzionale override dei limiti */
    xAxisMin: { type: [Number, null], default: null },
    xAxisMax: { type: [Number, null], default: null },
    y1AxisMin: { type: [Number, null], default: null },
    y1AxisMax: { type: [Number, null], default: null },
    y2AxisMin: { type: [Number, null], default: null },
    y2AxisMax: { type: [Number, null], default: null },

    color: { type: Array, default: () => colorPalette },
    axisLabelFormatter: { type: Function, default: format3 },
    tooltipFormatter: { type: Function, default: format3 },
  },
  data() {
    return { updateOptions: { notMerge: true } };
  },
  computed: {
    actualOption() {
      return {
        title: { text: this.title },
        legend: { type: "scroll" },
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
          min: this.xAxisMin ?? "dataMin",
          max: this.xAxisMax ?? "dataMax",
          axisLabel: { formatter: this.axisLabelFormatter, fontSize: 10 },
        },
        yAxis: [
          {
            name: this.y1AxisName,
            nameLocation: "middle",
            nameGap: 30,
            min: this.y1AxisMin ?? "dataMin",
            max: this.y1AxisMax ?? "dataMax",
            scale: true,
            axisLabel: { formatter: this.axisLabelFormatter, fontSize: 10 },
            alignTicks: false,
          },
          {
            name: this.y2AxisName,
            nameLocation: "middle",
            nameGap: 30,
            min: this.y2AxisMin ?? "dataMin",
            max: this.y2AxisMax ?? "dataMax",
            scale: true,
            axisLabel: { formatter: this.axisLabelFormatter, fontSize: 10 },
            alignTicks: false,
          },
        ],
        tooltip: {
          trigger: "axis",
          confine: true,
          formatter: (params) => {
            const rows = [
              `<strong>${params[0]?.axisValueLabel ?? ""}</strong>`,
            ];
            for (const p of params) {
              const yVal = Array.isArray(p.value) ? p.value[1] : p.value;
              rows.push(
                `${p.marker}${p.seriesName}: ${this.tooltipFormatter(yVal)}`
              );
            }
            return rows.join("<br/>");
          },
        },
        series: this.series.map((s) => merge({}, s, { showSymbol: false })),
        color: this.color,
      };
    },
  },
};
</script>
