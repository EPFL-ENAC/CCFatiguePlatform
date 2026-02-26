<template>
  <!-- Wrapper sets a fixed pixel height for ECharts (responsive width) -->
  <div :style="{ width: '100%', height: height + 'px' }">
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

/**
 * Register only the ECharts modules we need.
 * This keeps bundle size smaller than importing full ECharts.
 */
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
    /**
     * NOTE: aspectRatio is kept for backward compatibility,
     * but the component currently uses `height` (px) for sizing.
     */
    aspectRatio: { type: Number, default: 1 },

    /** ECharts series array (line/scatter/etc.). */
    series: { type: Array, default: () => [] },

    /** Chart title displayed above the plot. */
    title: { type: String, default: "" },

    /** Axis labels (also used in tooltip logic for some special cases). */
    xAxisName: { type: String, default: "" },
    yAxisName: { type: String, default: "" },

    /** Axis scale type: "value" | "category" | "log" (y only) etc. */
    xAxisType: { type: String, default: "value" },
    yAxisType: { type: String, default: "value" },

    /**
     * NOTE: dataZoom prop exists but we currently always enable inside zoom
     * on both axes (see `dataZoom` in computed option).
     */
    dataZoom: { type: String, default: "" },

    /** Global color palette (one color per series by default). */
    color: { type: Array, default: () => colorPalette },

    /** Optional explicit axis bounds (null => use dataMin/dataMax defaults). */
    xAxisMin: { type: [Number, null], default: null },
    xAxisMax: { type: [Number, null], default: null },
    yAxisMax: { type: [Number, null], default: null },
    yAxisMin: { type: [Number, null], default: null },

    /** Optional tick interval for linear y-axis. Ignored for log scale. */
    yAxisInterval: { type: [Number, null], default: null },

    /** Fixed chart height in px. Width is always 100% and autoresize is enabled. */
    height: { type: Number, default: 480 },

    /**
     * Axis tick label formatter.
     * Should accept a number and return a string. Default: format3.
     */
    axisLabelFormatter: {
      type: Function,
      default: format3,
    },

    /** Show/hide legend. */
    showLegend: { type: Boolean, default: true },

    /**
     * Tooltip value formatter (Y values).
     * Should accept a number and return a string. Default: format3.
     */
    tooltipFormatter: {
      type: Function,
      default: format3,
    },
  },
  data() {
    return {
      /**
       * ECharts update strategy:
       * notMerge=true ensures a clean redraw when option changes.
       * Useful when axis types or bounds change (avoids stale state).
       */
      updateOptions: {
        notMerge: true,
      },
    };
  },
  computed: {
    /**
     * Build the ECharts option object.
     * This is the single source of truth for chart appearance and behavior.
     */
    actualOption() {
      // For log scale, compute safe min/max (avoid <= 0 values).
      const yLogLimits =
        this.yAxisType === "log" ? computeLogYAxisLimits(this.series) : null;

      return {
        title: {
          text: this.title,
        },

        // Scroll legend is useful when many series are displayed.
        legend: this.showLegend ? { type: "scroll" } : { show: false },

        /**
         * Grid padding: leave room for axis titles and labels.
         * containLabel=true ensures labels stay inside the canvas.
         */
        grid: {
          left: 60,
          top: 40,
          right: 50,
          bottom: 50,
          containLabel: true,
        },

        xAxis: {
          type: this.xAxisType,
          name: this.xAxisName,
          nameLocation: "middle",
          // Increase gap so the x-axis title is not too close to tick labels.
          nameGap: 34,
          nameTextStyle: {
            fontSize: 20,
            fontWeight: "bold",
          },
          // If bounds are not provided, let ECharts use dataMin/dataMax.
          min: this.xAxisMin != null ? this.xAxisMin : "dataMin",
          max: this.xAxisMax != null ? this.xAxisMax : "dataMax",
          logBase: this.xAxisType === "log" ? 10 : undefined,
          axisLabel: {
            fontSize: 18,
            formatter: (val) => {
              if (this.xAxisType === "log") {
                const exp = Math.log10(val);
                const expRounded = Math.round(exp);
                if (Math.abs(exp - expRounded) < 1e-10)
                  return `10^${expRounded}`;
                return "";
              }

              const n = Number(val);
              if (!Number.isFinite(n)) return String(val);
              return Math.round(n).toLocaleString(undefined, {
                maximumFractionDigits: 0,
                useGrouping: false,
              });
            },
          },
        },

        yAxis: {
          type: this.yAxisType,
          name: this.yAxisName,
          nameLocation: "middle",
          // Bigger gap to improve readability (especially with large ticks).
          nameGap: 50,
          nameTextStyle: {
            fontSize: 20,
            fontWeight: 400,
          },

          // Log axis configuration
          logBase: this.yAxisType === "log" ? 10 : undefined,
          minorSplitLine: { show: this.yAxisType === "log" },

          /**
           * Axis bounds:
           * - log: use computed safe limits
           * - linear: use provided props or sensible defaults
           */
          min:
            this.yAxisType === "log"
              ? yLogLimits.min
              : this.yAxisMin != null
              ? this.yAxisMin
              : 0,

          max:
            this.yAxisType === "log"
              ? yLogLimits.max
              : (value) => {
                  const autoMax = value.max * 1.05;
                  return this.yAxisMax != null
                    ? Math.max(this.yAxisMax, autoMax)
                    : autoMax;
                },
          // Tick interval applies only to linear scale.
          interval:
            this.yAxisType === "log"
              ? undefined
              : this.yAxisInterval != null
              ? this.yAxisInterval
              : undefined,

          // Disable "nice" scaling on log axes to avoid unexpected bounds.
          scale: this.yAxisType === "log" ? false : undefined,
          nice: false,
          boundaryGap: this.yAxisType === "log" ? false : undefined,

          axisLabel: {
            formatter: (val) => {
              if (this.yAxisType === "log") {
                return `10^${Math.round(Math.log10(val))}`;
              }

              const n = Number(val);
              if (!Number.isFinite(n)) return val;

              return Math.round(n); // ← plus de décimales
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

          /**
           * Tooltip formatting:
           * - In some cycle graphs, x-values are internally normalized/logged.
           *   If a series provides `rawX`, we use it so the tooltip shows the real value.
           * - Otherwise, default ECharts axisValueLabel is used.
           */
          formatter: (params) => {
            const formatter = this.tooltipFormatter;
            let xLabel;

            // Special case: show raw (untransformed) cycles when available.
            if (
              this.xAxisName.includes("Number of cycles") ||
              this.xAxisName.includes("Normalized cycles")
            ) {
              const p = params[0];
              let rawX = null;

              // Convention: series can optionally provide rawX[] aligned with data indices.
              if (p.seriesIndex != null && this.series[p.seriesIndex]?.rawX) {
                const pointIndex = p.dataIndex;
                rawX = this.series[p.seriesIndex].rawX[pointIndex];
              }

              if (rawX != null) {
                xLabel = Number(rawX).toLocaleString(undefined, {
                  minimumFractionDigits: 0,
                  maximumFractionDigits: 0,
                  useGrouping: false,
                });
              } else {
                // Fallback if rawX is not available
                const xValue = p.value[0] ?? p.value;
                xLabel = Number(xValue).toLocaleString(undefined, {
                  minimumFractionDigits: 0,
                  maximumFractionDigits: 0,
                  useGrouping: false,
                });
              }
            } else {
              // Default behavior for other charts
              xLabel = params[0].axisValueLabel;
            }

            const rows = [`<strong>${xLabel}</strong>`];

            // Add one line per series at this x-position
            for (const p of params) {
              const rawY = Array.isArray(p.value) ? p.value[1] : p.value;
              const yVal = Number(rawY);
              const formattedY = formatter(yVal);
              rows.push(`${p.marker}${p.seriesName}: ${formattedY}`);
            }

            return rows.join("<br/>");
          },
        },

        /**
         * Inside zoom:
         * - Works with wheel/pinch and is unobtrusive (no visible slider).
         * - filterMode="none" keeps all points, only changes viewport.
         */
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

        /**
         * Force showSymbol=false unless a series explicitly overrides it.
         * Improves readability and performance for large datasets.
         */
        series: this.series.map((s) => ({
          ...s,
          showSymbol: s.showSymbol ?? false,
        })),

        color: this.color,
      };
    },
  },
  methods: {
    /**
     * Access the underlying ECharts instance (imperative API).
     * Useful for advanced operations (resize, dispatchAction, getDataURL, etc.).
     */
    getChartInstance() {
      return this.$refs.chartInstance?.chart;
    },
  },
};
</script>
