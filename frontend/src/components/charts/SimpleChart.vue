<template>
  <div :style="{ width: '100%', height: `${height}px` }">
    <v-chart
      ref="chartInstance"
      autoresize
      :option="actualOption"
      :update-options="updateOptions"
    />
  </div>
</template>

<script>
import { computeLogYAxisLimits, format3 } from "@/utils/formatters";
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
    yAxisType: { type: String, default: "value" },
    dataZoom: { type: String, default: "" },
    color: { type: Array, default: () => colorPalette },
    xAxisMin: { type: [Number, null], default: null },
    xAxisMax: { type: [Number, null], default: null },
    yAxisMax: { type: [Number, null], default: null },
    yAxisMin: { type: [Number, null], default: null },
    yAxisInterval: { type: [Number, null], default: null },
    showXAxisSlider: { type: Boolean, default: false },
    xSliderStart: { type: Number, default: 0 },
    xSliderEnd: { type: Number, default: 100 },
    height: { type: Number, default: 480 },
    axisLabelFormatter: {
      type: Function,
      default: format3,
    },
    showLegend: { type: Boolean, default: true },
    tooltipFormatter: {
      type: Function,
      default: format3,
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
      const isXLog = this.xAxisType === "log";
      const isYLog = this.yAxisType === "log";
      const yLogLimits = isYLog ? computeLogYAxisLimits(this.series) : null;

      return {
        title: this.title ? { text: this.title } : undefined,
        legend: this.showLegend ? { type: "scroll" } : { show: false },

        grid: {
          left: 60,
          top: 40,
          right: 90,
          bottom: this.showXAxisSlider ? 95 : 50,
          containLabel: true,
        },

        xAxis: {
          type: this.xAxisType,
          name: this.xAxisName,
          nameLocation: "middle",
          nameGap: 34,
          nameTextStyle: {
            fontSize: 20,
          },
          min: this.xAxisMin != null ? this.xAxisMin : "dataMin",
          max: this.xAxisMax != null ? this.xAxisMax : "dataMax",
          logBase: isXLog ? 10 : undefined,
          axisLabel: {
            formatter: (value) => {
              if (isXLog) {
                const exp = Math.log10(Number(value));
                return Math.abs(exp - Math.round(exp)) < 1e-8
                  ? `10^${Math.round(exp)}`
                  : "";
              }

              const n = Number(value);
              if (!Number.isFinite(n)) return String(value);

              if (Math.abs(n) >= 1e6) {
                const exp = Math.floor(Math.log10(Math.abs(n)));
                const mantissa = n / Math.pow(10, exp);
                const roundedMantissa =
                  Math.abs(mantissa) >= 10
                    ? mantissa.toFixed(0)
                    : mantissa.toFixed(1).replace(/\.0$/, "");

                return `${roundedMantissa}e${exp}`;
              }

              if (Math.abs(n) >= 1000) {
                return n.toLocaleString(undefined, {
                  maximumFractionDigits: 0,
                  useGrouping: false,
                });
              }

              if (Math.abs(n) >= 1) {
                return n.toLocaleString(undefined, {
                  minimumFractionDigits: 0,
                  maximumFractionDigits: 2,
                  useGrouping: false,
                });
              }

              return n.toLocaleString(undefined, {
                minimumFractionDigits: 0,
                maximumFractionDigits: 4,
                useGrouping: false,
              });
            },
            hideOverlap: false,
            showMaxLabel: true,
            showMinLabel: true,
            margin: 12,
            fontSize: 18,
            fontFamily: "Arial, sans-serif",
          },
        },
        yAxis: {
          type: this.yAxisType,
          name: this.yAxisName,
          nameLocation: "middle",
          nameGap: 50,
          nameTextStyle: {
            fontSize: 20,
            fontWeight: 400,
          },
          logBase: isYLog ? 10 : undefined,
          minorSplitLine: { show: isYLog },
          min: isYLog
            ? yLogLimits.min
            : this.yAxisMin != null
            ? this.yAxisMin
            : 0,
          max: isYLog
            ? yLogLimits.max
            : this.yAxisMax != null
            ? this.yAxisMax
            : (value) => value.max * 1.05,
          interval:
            !isYLog && this.yAxisInterval != null
              ? this.yAxisInterval
              : undefined,
          scale: isYLog ? false : undefined,
          nice: false,
          boundaryGap: isYLog ? false : undefined,
          axisLabel: {
            fontFamily: "Arial, sans-serif",
            formatter: (value) => {
              if (isYLog) {
                return `10^${Math.round(Math.log10(value))}`;
              }

              const n = Number(value);
              if (!Number.isFinite(n)) return value;

              return Math.round(n);
            },
            hideOverlap: true,
            showMaxLabel: true,
            showMinLabel: true,
            fontSize: 18,
          },
        },
        tooltip: {
          trigger: "axis",
          confine: true,
          formatter: (params) => {
            const formatter = this.tooltipFormatter;
            let xLabel;

            if (
              this.xAxisName.includes("Number of cycles") ||
              this.xAxisName.includes("Normalized cycles")
            ) {
              const firstParam = params[0];
              let rawX = null;

              if (
                firstParam.seriesIndex != null &&
                this.series[firstParam.seriesIndex]?.rawX
              ) {
                rawX =
                  this.series[firstParam.seriesIndex].rawX[
                    firstParam.dataIndex
                  ];
              }

              const xValue =
                rawX != null
                  ? rawX
                  : Array.isArray(firstParam.value)
                  ? firstParam.value[0]
                  : firstParam.value;

              xLabel = Number(xValue).toLocaleString(undefined, {
                minimumFractionDigits: 0,
                maximumFractionDigits: 0,
                useGrouping: false,
              });
            } else {
              xLabel = params[0].axisValueLabel;
            }

            const rows = [`<strong>${xLabel}</strong>`];

            for (const param of params) {
              const rawY = Array.isArray(param.value)
                ? param.value[1]
                : param.value;
              rows.push(
                `${param.marker}${param.seriesName}: ${formatter(Number(rawY))}`
              );
            }

            return rows.join("<br/>");
          },
        },

        dataZoom: [
          {
            id: "dataZoomXInside",
            xAxisIndex: [0],
            filterMode: "none",
            type: "inside",
          },
          {
            id: "dataZoomYInside",
            yAxisIndex: [0],
            filterMode: "none",
            type: "inside",
          },
          ...(this.showXAxisSlider
            ? [
                {
                  id: "dataZoomXSlider",
                  type: "slider",
                  xAxisIndex: [0],
                  filterMode: "none",
                  bottom: 20,
                  height: 30,
                  start: this.xSliderStart,
                  end: this.xSliderEnd,
                  showDetail: true,
                  labelFormatter: (value) => {
                    if (isXLog) {
                      const exp = Math.log10(Number(value));
                      return Math.abs(exp - Math.round(exp)) < 1e-8
                        ? `10^${Math.round(exp)}`
                        : Number(value).toExponential(1);
                    }

                    const n = Number(value);
                    if (!Number.isFinite(n)) return String(value);

                    if (Math.abs(n) >= 1e6) {
                      const exp = Math.floor(Math.log10(Math.abs(n)));
                      const mantissa = n / Math.pow(10, exp);
                      const roundedMantissa =
                        Math.abs(mantissa) >= 10
                          ? mantissa.toFixed(0)
                          : mantissa.toFixed(1).replace(/\.0$/, "");
                      return `${roundedMantissa}e${exp}`;
                    }

                    return n.toLocaleString(undefined, {
                      maximumFractionDigits: 0,
                      useGrouping: false,
                    });
                  },
                },
              ]
            : []),
        ],

        series: this.series.map((seriesItem) => ({
          ...seriesItem,
          showSymbol: seriesItem.showSymbol ?? false,
        })),

        color: this.color,
      };
    },
  },
  methods: {
    getChartInstance() {
      return this.$refs.chartInstance?.chart;
    },
  },
};
</script>
