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
import {
  computeAxisBoundsFromValues,
  computeLogAxisBoundsFromValues,
  format3,
  formatAxisName,
  formatAxisTick,
  getAxisScaleExponent,
  getSeriesDimensionValues,
  shouldUseConsistentScientificNotation,
  snapLogAxisMax,
  snapLogAxisMin,
} from "@/utils/formatters";
import { colorPalette } from "@/utils/style";
import { LineChart, ScatterChart } from "echarts/charts";
import {
  DataZoomComponent,
  GridComponent,
  LegendComponent,
  MarkLineComponent,
  MarkPointComponent,
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
  MarkLineComponent,
  MarkPointComponent,
  TitleComponent,
  TooltipComponent,
]);

const CHART_FONT_FAMILY = "Arial, sans-serif";
const AXIS_NAME_TEXT_STYLE = {
  fontFamily: CHART_FONT_FAMILY,
  fontSize: 20,
  fontWeight: 400,
};
const AXIS_LABEL_TEXT_STYLE = {
  fontFamily: CHART_FONT_FAMILY,
  fontSize: 18,
  fontWeight: 400,
};
const TITLE_TEXT_STYLE = {
  fontFamily: CHART_FONT_FAMILY,
  fontSize: 18,
  fontWeight: 400,
};
const LEGEND_TEXT_STYLE = {
  fontFamily: CHART_FONT_FAMILY,
  fontSize: 12,
  fontWeight: 400,
};

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
    yAxisSplitNumber: { type: [Number, null], default: null },
    showXAxisSlider: { type: Boolean, default: false },
    xSliderStart: { type: Number, default: 0 },
    xSliderEnd: { type: Number, default: 100 },
    cycleCountXAxis: { type: Boolean, default: false },
    normalizedXAxis: { type: Boolean, default: false },
    normalizedYAxis: { type: Boolean, default: false },
    // Opt-in, defaults to false everywhere else: skips the automatic
    // "(x10^n)" axis-name suffix + scaled tick values that otherwise kick
    // in once any Y value reaches >=1000 (see getAxisScaleExponent). Pair
    // with axisLabelFormatter for fully plain tick labels too - this prop
    // alone only fixes the axis name/scaling, not the per-tick formatting.
    yAxisPlain: { type: Boolean, default: false },
    height: { type: Number, default: 480 },
    axisLabelFormatter: {
      type: Function,
      default: null,
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
    isXLog() {
      return this.xAxisType === "log";
    },
    isYLog() {
      return this.yAxisType === "log";
    },
    isNormalizedXAxis() {
      return (
        this.normalizedXAxis ||
        this.xAxisName.toLowerCase().includes("normalized")
      );
    },
    isCycleCountXAxis() {
      const axisName = this.xAxisName.toLowerCase();
      return (
        !this.isXLog &&
        !this.isNormalizedXAxis &&
        (this.cycleCountXAxis ||
          axisName.includes("cycle count") ||
          axisName.includes("number of cycles"))
      );
    },
    isNormalizedYAxis() {
      return (
        this.normalizedYAxis ||
        this.yAxisName.toLowerCase().includes("normalized")
      );
    },
    xAxisValues() {
      return getSeriesDimensionValues(this.series, 0);
    },
    yAxisValues() {
      return getSeriesDimensionValues(this.series, 1);
    },
    xAxisBounds() {
      if (!this.xAxisValues.length) return { min: null, max: null };
      return this.isXLog
        ? computeLogAxisBoundsFromValues(this.xAxisValues)
        : computeAxisBoundsFromValues(this.xAxisValues);
    },
    yAxisBounds() {
      if (!this.yAxisValues.length) return { min: null, max: null };
      return this.isYLog
        ? computeLogAxisBoundsFromValues(this.yAxisValues)
        : computeAxisBoundsFromValues(this.yAxisValues);
    },
    shouldStartLogXAxisAt100() {
      return (
        this.isXLog &&
        this.xAxisValues.length > 0 &&
        !this.xAxisValues.some((value) => value > 0 && value < 100)
      );
    },
    effectiveXAxisMin() {
      if (this.isNormalizedXAxis) return 0;
      if (this.shouldStartLogXAxisAt100) return 100;
      if (this.xAxisMin != null) {
        if (!this.isXLog) return this.xAxisMin;
        return snapLogAxisMin(Math.max(this.xAxisMin, 1e-12)) ?? 1e-12;
      }
      if (this.xAxisBounds.min != null) return this.xAxisBounds.min;
      return "dataMin";
    },
    effectiveXAxisMax() {
      if (this.isNormalizedXAxis) return 1;
      if (this.xAxisMax != null) {
        if (!this.isXLog) return this.xAxisMax;
        return snapLogAxisMax(Math.max(this.xAxisMax, 1e-12)) ?? 1e-12;
      }
      if (this.xAxisBounds.max != null) return this.xAxisBounds.max;
      return "dataMax";
    },
    effectiveYAxisMin() {
      if (this.isNormalizedYAxis) return 0;
      if (this.yAxisMin != null) {
        if (!this.isYLog) return this.yAxisMin;
        return snapLogAxisMin(Math.max(this.yAxisMin, 1e-12)) ?? 1e-12;
      }
      if (this.isYLog && this.yAxisBounds.min != null) {
        return this.yAxisBounds.min;
      }
      return this.isYLog ? "dataMin" : 0;
    },
    effectiveYAxisMax() {
      if (this.isNormalizedYAxis) return this.normalizedYAxisMax;
      if (this.yAxisMax != null) {
        if (!this.isYLog) return this.yAxisMax;
        return snapLogAxisMax(Math.max(this.yAxisMax, 1e-12)) ?? 1e-12;
      }
      if (this.yAxisBounds.max != null) return this.yAxisBounds.max;
      return this.isYLog ? "dataMax" : (value) => value.max * 1.05;
    },
    normalizedYAxisMax() {
      const maxValue = Math.max(
        0,
        ...this.yAxisValues.filter((value) => Number.isFinite(value))
      );

      if (maxValue <= 1) return 1;

      const roundedMax = Math.ceil(maxValue / 0.2) * 0.2;
      return Number(Math.max(1.2, roundedMax).toFixed(10));
    },
    normalizedYAxisSplitNumber() {
      return Math.round(this.normalizedYAxisMax / 0.2);
    },
    effectiveYAxisInterval() {
      if (this.isNormalizedYAxis) return 0.2;
      if (!this.isYLog && this.yAxisInterval != null) return this.yAxisInterval;
      return undefined;
    },
    effectiveYAxisSplitNumber() {
      if (this.isNormalizedYAxis) return this.normalizedYAxisSplitNumber;
      if (this.yAxisSplitNumber != null) return this.yAxisSplitNumber;
      return undefined;
    },
    xAxisScaleExponent() {
      if (this.isXLog || this.isNormalizedXAxis || this.isCycleCountXAxis) {
        return null;
      }
      return getAxisScaleExponent(
        this.xAxisValues,
        this.effectiveXAxisMin,
        this.effectiveXAxisMax
      );
    },
    yAxisScaleExponent() {
      if (this.isYLog || this.isNormalizedYAxis || this.yAxisPlain) {
        return null;
      }
      return getAxisScaleExponent(
        this.yAxisValues,
        this.effectiveYAxisMin,
        this.effectiveYAxisMax
      );
    },
    formattedXAxisName() {
      return formatAxisName(this.xAxisName, this.xAxisScaleExponent);
    },
    formattedYAxisName() {
      return formatAxisName(this.yAxisName, this.yAxisScaleExponent);
    },
    xAxisForceScientific() {
      return shouldUseConsistentScientificNotation(
        this.xAxisValues,
        this.effectiveXAxisMin,
        this.effectiveXAxisMax,
        {
          cycleCount: this.isCycleCountXAxis,
          log: this.isXLog,
          normalized: this.isNormalizedXAxis,
          scaleExponent: this.xAxisScaleExponent,
        }
      );
    },
    yAxisForceScientific() {
      return shouldUseConsistentScientificNotation(
        this.yAxisValues,
        this.effectiveYAxisMin,
        this.effectiveYAxisMax,
        {
          log: this.isYLog,
          normalized: this.isNormalizedYAxis,
          scaleExponent: this.yAxisScaleExponent,
        }
      );
    },

    actualOption() {
      return {
        title: this.title
          ? { text: this.title, textStyle: TITLE_TEXT_STYLE }
          : undefined,
        legend: this.showLegend
          ? { type: "scroll", textStyle: LEGEND_TEXT_STYLE }
          : { show: false },

        grid: {
          left: 60,
          top: 40,
          right: 90,
          bottom: this.showXAxisSlider ? 95 : 50,
          containLabel: true,
        },

        xAxis: {
          type: this.xAxisType,
          name: this.formattedXAxisName,
          nameLocation: "middle",
          nameGap: 34,
          nameTextStyle: AXIS_NAME_TEXT_STYLE,
          min: this.effectiveXAxisMin,
          max: this.effectiveXAxisMax,
          interval: this.isNormalizedXAxis ? 0.2 : undefined,
          minInterval: this.isNormalizedXAxis ? 0.2 : undefined,
          maxInterval: this.isNormalizedXAxis ? 0.2 : undefined,
          splitNumber: this.isNormalizedXAxis ? 5 : undefined,
          logBase: this.isXLog ? 10 : undefined,
          axisLabel: {
            ...AXIS_LABEL_TEXT_STYLE,
            formatter: this.formatXAxisTick,
            hideOverlap: false,
            showMaxLabel: true,
            showMinLabel: true,
            margin: 12,
          },
        },
        yAxis: {
          type: this.yAxisType,
          name: this.formattedYAxisName,
          nameLocation: "middle",
          nameGap: 50,
          nameTextStyle: AXIS_NAME_TEXT_STYLE,
          logBase: this.isYLog ? 10 : undefined,
          minorSplitLine: { show: this.isYLog },
          min: this.effectiveYAxisMin,
          max: this.effectiveYAxisMax,
          interval: this.effectiveYAxisInterval,
          minInterval: this.isNormalizedYAxis ? 0.2 : undefined,
          maxInterval: this.isNormalizedYAxis ? 0.2 : undefined,
          splitNumber: this.effectiveYAxisSplitNumber,
          scale: true,
          boundaryGap: false,
          nice: false,
          axisLabel: {
            ...AXIS_LABEL_TEXT_STYLE,
            formatter: this.formatYAxisTick,
            hideOverlap: true,
            showMaxLabel: true,
            showMinLabel: true,
          },
        },
        tooltip: {
          trigger: "axis",
          confine: true,
          formatter: (params) => {
            const formatter = this.tooltipFormatter;
            // ECharts calls this with a single object (not an array) for
            // item-triggered tooltips, e.g. hovering a markLine/markPoint
            // directly rather than the underlying series - normalize so
            // numeric values always go through `formatter` below instead
            // of falling back to an unrounded raw value.
            const paramList = Array.isArray(params) ? params : [params];
            if (paramList.length === 0 || !paramList[0]) return "";
            let xLabel;

            if (
              this.xAxisName.toLowerCase().includes("number of cycles") ||
              this.xAxisName.toLowerCase().includes("cycle count") ||
              this.xAxisName.toLowerCase().includes("normalized cycles")
            ) {
              const firstParam = paramList[0];
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
              xLabel =
                paramList[0].axisValueLabel ??
                formatter(Number(paramList[0].axisValue));
            }

            const rows = [`<strong>${xLabel}</strong>`];

            for (const param of paramList) {
              const rawY = Array.isArray(param.value)
                ? param.value[1]
                : param.value;
              if (rawY == null) continue;
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
                    return this.formatXAxisTick(value);
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
    formatXAxisTick(value) {
      if (this.isXLog) return formatAxisTick(value, { log: true });
      if (this.isNormalizedXAxis) {
        return formatAxisTick(value, { normalized: true });
      }
      if (this.isCycleCountXAxis) {
        return formatAxisTick(value, {
          cycleCount: true,
          forceScientific: this.xAxisForceScientific,
        });
      }
      if (this.axisLabelFormatter) return this.axisLabelFormatter(value);
      return formatAxisTick(value, {
        forceScientific: this.xAxisForceScientific,
        scaleExponent: this.xAxisScaleExponent,
      });
    },
    formatYAxisTick(value) {
      if (this.isYLog) return formatAxisTick(value, { log: true });
      if (this.isNormalizedYAxis) {
        return formatAxisTick(value, { normalized: true });
      }
      if (this.axisLabelFormatter) return this.axisLabelFormatter(value);
      return formatAxisTick(value, {
        forceScientific: this.yAxisForceScientific,
        scaleExponent: this.yAxisScaleExponent,
      });
    },
    getChartInstance() {
      return this.$refs.chartInstance?.chart;
    },
  },
};
</script>
