<template>
  <div :style="{ width: '100%', height: `${height}px` }">
    <v-chart
      autoresize
      :option="actualOption"
      :update-options="updateOptions"
    />
  </div>
</template>

<script>
import {
  computeAxisBoundsFromValues,
  formatAxisTick,
  getSeriesDimensionValues,
} from "@/utils/formatters";
import { CustomChart, LineChart, ScatterChart } from "echarts/charts";
import {
  GridComponent,
  LegendComponent,
  TitleComponent,
  TooltipComponent,
} from "echarts/components";
import { use } from "echarts/core";
import { CanvasRenderer } from "echarts/renderers";
import VChart from "vue-echarts";

use([
  CanvasRenderer,
  CustomChart,
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
  fontSize: 16,
  fontWeight: 400,
};

const DEFAULT_INSIDE_COLOR = "#4caf50";
const DEFAULT_OUTSIDE_COLOR = "#f44336";

/**
 * 2D failure-envelope chart: a closed boundary curve (e.g. a Tsai-Hill or
 * Tsai-Wu envelope in stress space) filled as an enclosed region, plus an
 * optional applied operating point colored by whether it falls inside
 * (safe) or outside (unsafe) that region.
 *
 * The boundary fill uses an ECharts `custom` series with a polygon
 * renderItem, since a plain `line` series' areaStyle only fills toward the
 * axis baseline, not an arbitrary enclosed loop.
 */
export default {
  name: "EnvelopeChart",
  components: {
    VChart,
  },
  props: {
    // Array of [x, y] boundary points. Does not need to repeat the first
    // point at the end - the polygon and boundary line both close it.
    boundaryPoints: { type: Array, default: () => [] },
    // [x, y] applied operating point, or null to omit the marker.
    appliedPoint: { type: Array, default: null },
    // true = inside the envelope (safe), false = outside (unsafe), null =
    // unknown/not evaluated (marker rendered neutral).
    isInside: { type: Boolean, default: null },
    insideColor: { type: String, default: DEFAULT_INSIDE_COLOR },
    outsideColor: { type: String, default: DEFAULT_OUTSIDE_COLOR },
    xAxisName: { type: String, default: "" },
    yAxisName: { type: String, default: "" },
    // Additional raw ECharts series to overlay (e.g. a load-vector line).
    extraSeries: { type: Array, default: () => [] },
    height: { type: Number, default: 420 },
  },
  data() {
    return {
      updateOptions: { notMerge: true },
    };
  },
  computed: {
    markerColor() {
      if (this.isInside === true) return this.insideColor;
      if (this.isInside === false) return this.outsideColor;
      return "#757575";
    },
    closedBoundary() {
      if (this.boundaryPoints.length === 0) return [];
      const first = this.boundaryPoints[0];
      const last = this.boundaryPoints[this.boundaryPoints.length - 1];
      const alreadyClosed = first[0] === last[0] && first[1] === last[1];
      return alreadyClosed
        ? this.boundaryPoints
        : [...this.boundaryPoints, first];
    },
    boundsSeries() {
      const points = [...this.boundaryPoints];
      if (this.appliedPoint) points.push(this.appliedPoint);
      const series = [{ data: points }, ...this.extraSeries];
      // Envelopes are centered on stress space, so always include the
      // origin in the bounds even if all data happens to be one-sided.
      series.push({ data: [[0, 0]] });
      return series;
    },
    xAxisBounds() {
      return computeAxisBoundsFromValues(
        getSeriesDimensionValues(this.boundsSeries, 0)
      );
    },
    yAxisBounds() {
      return computeAxisBoundsFromValues(
        getSeriesDimensionValues(this.boundsSeries, 1)
      );
    },
    actualOption() {
      const boundary = this.closedBoundary;
      const insideColor = this.insideColor;

      const series = [
        {
          id: "envelope-fill",
          type: "custom",
          silent: true,
          renderItem: (params, api) => {
            const points = boundary.map((p) => api.coord(p));
            return {
              type: "polygon",
              shape: { points },
              style: {
                fill: insideColor,
                opacity: 0.12,
              },
            };
          },
          data: [[0, 0]],
          clip: false,
        },
        {
          id: "envelope-boundary",
          type: "line",
          name: "Envelope",
          data: boundary,
          showSymbol: false,
          lineStyle: { color: insideColor, width: 2 },
          itemStyle: { color: insideColor },
          z: 2,
        },
        ...this.extraSeries,
      ];

      if (this.appliedPoint) {
        series.push({
          id: "applied-point",
          type: "scatter",
          name: "Applied point",
          data: [this.appliedPoint],
          symbolSize: 14,
          itemStyle: {
            color: this.markerColor,
            borderColor: "#fff",
            borderWidth: 1,
          },
          z: 3,
        });
      }

      return {
        legend: { show: false },
        grid: { left: 60, top: 30, right: 30, bottom: 50, containLabel: true },
        xAxis: {
          type: "value",
          name: this.xAxisName,
          nameLocation: "middle",
          nameGap: 34,
          nameTextStyle: AXIS_NAME_TEXT_STYLE,
          min: this.xAxisBounds.min ?? -1,
          max: this.xAxisBounds.max ?? 1,
          interval:
            this.xAxisBounds.min !== this.xAxisBounds.max
              ? this.xAxisBounds.interval
              : undefined,
          axisLabel: {
            ...AXIS_LABEL_TEXT_STYLE,
            formatter: (value) => formatAxisTick(value),
          },
        },
        yAxis: {
          type: "value",
          name: this.yAxisName,
          nameLocation: "middle",
          nameGap: 50,
          nameTextStyle: AXIS_NAME_TEXT_STYLE,
          min: this.yAxisBounds.min ?? -1,
          max: this.yAxisBounds.max ?? 1,
          interval:
            this.yAxisBounds.min !== this.yAxisBounds.max
              ? this.yAxisBounds.interval
              : undefined,
          axisLabel: {
            ...AXIS_LABEL_TEXT_STYLE,
            formatter: (value) => formatAxisTick(value),
          },
        },
        tooltip: {
          trigger: "item",
          confine: true,
          formatter: (p) => {
            if (!Array.isArray(p.value)) return "";
            const [x, y] = p.value;
            return `${p.seriesName}<br/>x: ${x.toFixed(2)}, y: ${y.toFixed(2)}`;
          },
        },
        series,
      };
    },
  },
};
</script>
