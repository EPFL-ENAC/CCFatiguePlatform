<template>
  <!-- ECharts-GL needs an explicit container size (width 100% + fixed px height) -->
  <div ref="chart" :style="{ width: '100%', height: height + 'px' }" />
</template>

<script>
import * as echarts from "echarts";
import "echarts-gl";

function hexToRgb(hex) {
  const h = String(hex || "").replace("#", "");
  const full =
    h.length === 3
      ? h
          .split("")
          .map((c) => c + c)
          .join("")
      : h;
  const n = parseInt(full, 16);
  return { r: (n >> 16) & 255, g: (n >> 8) & 255, b: n & 255 };
}
function mixWithWhite(hex, t) {
  const { r, g, b } = hexToRgb(hex);
  const rr = Math.round(r + (255 - r) * t);
  const gg = Math.round(g + (255 - g) * t);
  const bb = Math.round(b + (255 - b) * t);
  return `rgb(${rr},${gg},${bb})`;
}

export default {
  name: "Markov3D",
  props: {
    /**
     * Data format (expected):
     * - Each item is: [i, j, z, xCenter?, yCenter?]
     *   where i/j are 0-based bin indices and z is the number of cycles.
     *
     * Note: The renderer shifts i/j by +1 because axes are configured as category [1..N].
     */
    data: { type: Array, required: true },

    /** Axis labels (units can be included by caller, e.g. "Stress range [MPa]") */
    xLabel: { type: String, default: "Range bin" },
    yLabel: { type: String, default: "Mean bin" },
    zLabel: { type: String, default: "Cycles" },

    /**
     * Optional physical bin centers (same length as matrix size).
     * If provided, they are used in axis labels and tooltip (in MPa here).
     */
    xCenters: { type: Array, default: () => [] },
    yCenters: { type: Array, default: () => [] },

    /** Title is currently hidden in option (show:false) but kept for future use */
    title: { type: String, default: "Markov matrix - 3D histogram" },

    /** Fixed chart height in px */
    height: { type: Number, default: 700 },

    /**
     * External key to force relayout when parent grid layout changes
     * (e.g., 1/2/3 columns). We only call resize().
     */
    resizeKey: { type: Number, default: 0 },
    baseColor: { type: String, default: "#d32f2f" },
  },

  data() {
    return {
      chart: null,
      selectedZRange: null,
    };
  },

  watch: {
    /** Re-render when data changes */
    data() {
      this.render();
    },

    /** Resize when layout changes (e.g., number of columns) */
    resizeKey() {
      this.$nextTick(() => {
        if (this.chart) this.chart.resize();
      });
    },

    /** Resize if height changes */
    height() {
      this.$nextTick(() => {
        if (this.chart) this.chart.resize();
      });
    },
  },

  mounted() {
    this.chart = echarts.init(this.$refs.chart);
    this.render();

    window.addEventListener("resize", this.onResize);

    this.chart.on("datarangeselected", (params) => {
      if (!params || !params.selected) return;

      this.selectedZRange = params.selected;
      this.render();
    });
  },

  beforeDestroy() {
    window.removeEventListener("resize", this.onResize);

    if (this.chart) {
      this.chart.off("datarangeselected");
      this.chart.dispose();
    }
  },

  methods: {
    /** Window resize handler */
    onResize() {
      if (this.chart) this.chart.resize();
    },

    /**
     * Render (or re-render) the full 3D option.
     * This component is intentionally "stateless": the option is rebuilt every time.
     */
    render() {
      if (!this.chart) return;

      const data = Array.isArray(this.data) ? this.data : [];

      // VisualMap bounds based on z-values
      const zs = data.map((d) => d[2]).filter(Number.isFinite);
      const zMin = zs.length ? Math.min(...zs) : 0;
      const rawZMax = zs.length ? Math.max(...zs) : 1;
      const zMax = niceSelectedMax(rawZMax);

      const currentRange =
        this.selectedZRange &&
        Array.isArray(this.selectedZRange) &&
        this.selectedZRange.length === 2
          ? this.selectedZRange
          : [zMin, zMax];

      const currentZMaxRaw =
        currentRange[1] > currentRange[0]
          ? currentRange[1]
          : currentRange[0] + 1;

      const currentZMax = niceSelectedMax(currentZMaxRaw);
      function niceSelectedMax(value) {
        if (!Number.isFinite(value) || value <= 0) return 1;

        if (value < 100) return Math.ceil(value / 10) * 10;
        if (value < 1000) return Math.ceil(value / 50) * 50;
        if (value < 5000) return Math.ceil(value / 100) * 100;
        if (value < 20000) return Math.ceil(value / 500) * 500;

        return Math.ceil(value / 1000) * 1000;
      }

      // Formatting helpers (MPa shown with one decimal)
      const fmtMpa = (v) => (Number.isFinite(v) ? Number(v).toFixed(1) : "-");
      const fmtInt = (v) =>
        Number.isFinite(v)
          ? Math.round(Number(v)).toLocaleString(undefined, {
              useGrouping: false,
            })
          : "-";

      // Axis categories are generated from centers length (fallback to 64)
      const xCount = this.xCenters?.length || 64;
      const yCount = this.yCenters?.length || 64;

      const option = {
        // Title currently hidden; keep prop for future toggles
        title: { show: false },

        /**
         * Tooltip shows:
         * - physical center value (MPa) when available
         * - bin index (i/j) for debugging
         * - cycles count (z)
         */
        tooltip: {
          formatter: (p) => {
            const v = p.value || [];
            // v[0], v[1] are category values (1-based), but we also carry original indices in the raw data.
            // Here we use the original indices stored in the unshifted array (i/j) when available.
            const i = v[0];
            const j = v[1];
            const z = v[2];

            // Prefer centers arrays, fallback to payload (d[3], d[4]) if present
            const xPhys = Number.isFinite(this.xCenters?.[i])
              ? this.xCenters[i]
              : v[3];
            const yPhys = Number.isFinite(this.yCenters?.[j])
              ? this.yCenters[j]
              : v[4];

            // Simple coloring for readability
            const cPhys = this.baseColor;
            const cBin = "#666";

            return [
              `${
                this.xLabel
              }: <span style="color:${cPhys};font-weight:700">${fmtMpa(
                xPhys
              )} MPa</span> <span style="color:${cBin}">(bin ${i})</span>`,
              `${
                this.yLabel
              }: <span style="color:${cPhys};font-weight:700">${fmtMpa(
                yPhys
              )} MPa</span> <span style="color:${cBin}">(bin ${j})</span>`,
              `${this.zLabel}: <b>${fmtInt(z)}</b>`,
            ].join("<br/>");
          },
        },

        /**
         * Color mapping for bars based on z (cycles).
         * calculable=true adds a draggable scale UI.
         */
        visualMap: {
          show: true,
          dimension: 2,
          calculable: true,
          left: 10,
          bottom: 10,
          min: zMin,
          max: zMax,
          range: currentRange,
          inRange: {
            color: [mixWithWhite(this.baseColor, 0.85), this.baseColor],
          },
        },
        /**
         * Axes are category-based:
         * - displayed categories are 1..N (human-friendly)
         * - we attach bin center values in labels (MPa)
         */
        xAxis3D: {
          type: "category",
          name: this.xLabel,
          nameGap: 25,
          nameTextStyle: { fontSize: 20 },

          // Categories are 1..N (NOT 0..N-1)
          data: Array.from({ length: xCount }, (_, i) => i + 1),

          axisLabel: {
            interval: 8,
            rotate: 35,
            showMaxLabel: true,
            formatter: (v) => {
              const idx = Number(v) - 1;
              const mp = this.xCenters?.[idx];

              return `{bin|${v}} {mpa|(${fmtMpa(mp)} MPa)}`;
            },
            rich: {
              bin: {
                color: "#000",
                fontSize: 16,
                fontWeight: 400,
              },
              mpa: {
                color: this.baseColor,
                fontSize: 16,
                fontWeight: 400,
              },
            },
          },
        },

        yAxis3D: {
          type: "category",
          name: this.yLabel,
          nameGap: 25,
          nameTextStyle: { fontSize: 20 },

          data: Array.from({ length: yCount }, (_, j) => j + 1),

          axisLabel: {
            interval: 8,
            rotate: -35,
            showMaxLabel: true,
            formatter: (v) => {
              const idx = Number(v) - 1;
              const mp = this.yCenters?.[idx];

              return `{bin|${v}} {mpa|(${fmtMpa(mp)} MPa)}`;
            },
            rich: {
              bin: {
                color: "#000",
                fontSize: 16,
                fontWeight: 400,
              },
              mpa: {
                color: this.baseColor,
                fontSize: 16,
                fontWeight: 400,
              },
            },
          },
          splitLine: { show: true },
        },

        zAxis3D: {
          name: this.zLabel,
          min: 0,
          max: currentZMax,
          nameGap: 20,
          nameTextStyle: { fontSize: 20 },
          axisLabel: {
            formatter: (v) => Math.round(v),
            textStyle: {
              fontSize: 18,
            },
          },
        },

        /**
         * 3D scene configuration:
         * - box sizes tune the aspect of the rendered cube
         * - viewControl sets the default camera angle/distance
         * - lighting improves depth perception
         */
        grid3D: {
          boxWidth: 70,
          boxDepth: 70,
          boxHeight: 70,

          // Leave room at bottom for labels/visualMap
          top: -100,
          bottom: 300,

          viewControl: {
            alpha: 12,
            beta: 45,
            distance: 150,
          },

          light: {
            main: { intensity: 1.2 },
            ambient: { intensity: 0.4 },
          },
        },

        series: [
          {
            type: "bar3D",
            shading: "lambert",

            /**
             * Shift indices by +1 because xAxis3D/yAxis3D categories are 1..N.
             * Input data indices are expected 0..N-1.
             */
            data: data.map((d) => [d[0] + 1, d[1] + 1, d[2]]),

            // Bar size close to 1 fills the "bin cell" visually
            barSize: 0.95,
          },
        ],
      };

      // Replace full option (true) for consistent redraw
      this.chart.setOption(option, true);
    },
  },
};
</script>
