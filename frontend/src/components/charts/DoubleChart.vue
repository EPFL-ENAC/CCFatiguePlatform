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
    cycleCountXAxis: { type: Boolean, default: false },
    normalizedXAxis: { type: Boolean, default: false },
    axisLabelFormatter: { type: Function, default: null },
    tooltipFormatter: { type: Function, default: format3 },
  },
  data() {
    return { updateOptions: { notMerge: true } };
  },
  computed: {
    isXLog() {
      return this.xAxisType === "log";
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
    xAxisValues() {
      return getSeriesDimensionValues(this.series, 0);
    },
    y1Series() {
      return this.series.filter(
        (seriesItem) => (seriesItem.yAxisIndex ?? 0) === 0
      );
    },
    y2Series() {
      return this.series.filter((seriesItem) => seriesItem.yAxisIndex === 1);
    },
    y1AxisValues() {
      return getSeriesDimensionValues(this.y1Series, 1);
    },
    y2AxisValues() {
      return getSeriesDimensionValues(this.y2Series, 1);
    },
    xAxisBounds() {
      if (!this.xAxisValues.length) return { min: null, max: null };
      return this.isXLog
        ? computeLogAxisBoundsFromValues(this.xAxisValues)
        : computeAxisBoundsFromValues(this.xAxisValues);
    },
    y1AxisBounds() {
      return computeAxisBoundsFromValues(this.y1AxisValues);
    },
    y2AxisBounds() {
      return computeAxisBoundsFromValues(this.y2AxisValues);
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
    effectiveY1AxisMin() {
      if (this.y1AxisMin != null) return this.y1AxisMin;
      return this.y1AxisBounds.min ?? "dataMin";
    },
    effectiveY1AxisMax() {
      if (this.y1AxisMax != null) return this.y1AxisMax;
      return this.y1AxisBounds.max ?? "dataMax";
    },
    effectiveY2AxisMin() {
      if (this.y2AxisMin != null) return this.y2AxisMin;
      return this.y2AxisBounds.min ?? "dataMin";
    },
    effectiveY2AxisMax() {
      if (this.y2AxisMax != null) return this.y2AxisMax;
      return this.y2AxisBounds.max ?? "dataMax";
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
    y1AxisScaleExponent() {
      return getAxisScaleExponent(
        this.y1AxisValues,
        this.effectiveY1AxisMin,
        this.effectiveY1AxisMax
      );
    },
    y2AxisScaleExponent() {
      return getAxisScaleExponent(
        this.y2AxisValues,
        this.effectiveY2AxisMin,
        this.effectiveY2AxisMax
      );
    },
    formattedXAxisName() {
      return formatAxisName(this.xAxisName, this.xAxisScaleExponent);
    },
    formattedY1AxisName() {
      return formatAxisName(this.y1AxisName, this.y1AxisScaleExponent);
    },
    formattedY2AxisName() {
      return formatAxisName(this.y2AxisName, this.y2AxisScaleExponent);
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
    y1AxisForceScientific() {
      return shouldUseConsistentScientificNotation(
        this.y1AxisValues,
        this.effectiveY1AxisMin,
        this.effectiveY1AxisMax,
        { scaleExponent: this.y1AxisScaleExponent }
      );
    },
    y2AxisForceScientific() {
      return shouldUseConsistentScientificNotation(
        this.y2AxisValues,
        this.effectiveY2AxisMin,
        this.effectiveY2AxisMax,
        { scaleExponent: this.y2AxisScaleExponent }
      );
    },
    actualOption() {
      return {
        title: this.title
          ? { text: this.title, textStyle: TITLE_TEXT_STYLE }
          : undefined,
        legend: { type: "scroll", textStyle: LEGEND_TEXT_STYLE },
        grid: {
          left: 60,
          top: 40,
          right: 90,
          bottom: 50,
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
            margin: 12,
            showMaxLabel: true,
            showMinLabel: true,
          },
        },
        yAxis: [
          {
            name: this.formattedY1AxisName,
            nameLocation: "middle",
            nameGap: 50,
            nameTextStyle: AXIS_NAME_TEXT_STYLE,
            min: this.effectiveY1AxisMin,
            max: this.effectiveY1AxisMax,
            scale: true,
            boundaryGap: false,
            nice: false,
            axisLabel: {
              ...AXIS_LABEL_TEXT_STYLE,
              formatter: (value) => this.formatYAxisTick(value, 0),
              hideOverlap: true,
              showMaxLabel: true,
              showMinLabel: true,
            },
            alignTicks: false,
          },
          {
            name: this.formattedY2AxisName,
            nameLocation: "middle",
            nameGap: 50,
            nameTextStyle: AXIS_NAME_TEXT_STYLE,
            min: this.effectiveY2AxisMin,
            max: this.effectiveY2AxisMax,
            scale: true,
            boundaryGap: false,
            nice: false,
            axisLabel: {
              ...AXIS_LABEL_TEXT_STYLE,
              formatter: (value) => this.formatYAxisTick(value, 1),
              hideOverlap: true,
              showMaxLabel: true,
              showMinLabel: true,
            },
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
    formatYAxisTick(value, axisIndex) {
      if (this.axisLabelFormatter) return this.axisLabelFormatter(value);
      return formatAxisTick(value, {
        forceScientific:
          axisIndex === 0
            ? this.y1AxisForceScientific
            : this.y2AxisForceScientific,
        scaleExponent:
          axisIndex === 0 ? this.y1AxisScaleExponent : this.y2AxisScaleExponent,
      });
    },
  },
};
</script>
