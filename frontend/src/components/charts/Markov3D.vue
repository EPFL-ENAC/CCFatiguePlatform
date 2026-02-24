<template>
  <!-- ECharts-GL needs an explicit container size (width 100% + fixed px height) -->
  <div ref="chart" :style="{ width: '100%', height: height + 'px' }" />
</template>

<script>
import * as echarts from "echarts";
import "echarts-gl";

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
  },

  data() {
    return { chart: null };
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
    // Create chart instance once the DOM node exists
    this.chart = echarts.init(this.$refs.chart);
    this.render();

    // Keep responsive behavior when window resizes
    window.addEventListener("resize", this.onResize);
  },

  beforeDestroy() {
    window.removeEventListener("resize", this.onResize);
    if (this.chart) this.chart.dispose();
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
      const zMax = zs.length ? Math.max(...zs) : 1;

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
            const cPhys = "#d32f2f";
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
          dimension: 2, // z is at index 2 in series data
          calculable: true,
          left: 10,
          bottom: 10,
          min: zMin,
          max: zMax,
        },

        /**
         * Axes are category-based:
         * - displayed categories are 1..N (human-friendly)
         * - we attach bin center values in labels (MPa)
         */
        xAxis3D: {
          type: "category",
          name: this.xLabel,
          nameGap: 20,
          nameTextStyle: { fontSize: 16 },

          // Categories are 1..N (NOT 0..N-1)
          data: Array.from({ length: xCount }, (_, i) => i + 1),

          axisLabel: {
            // Show one label every ~8 bins to avoid clutter
            interval: 8,
            rotate: 35,
            showMaxLabel: true,

            // Rich label shows: "bin" + "(MPa)"
            formatter: (v) => {
              const idx = Number(v) - 1;
              const mp = this.xCenters?.[idx];
              return `{bin|${v}} {mpa|(${fmtMpa(mp)} MPa)}`;
            },
            rich: {
              bin: { fontWeight: 600, fontSize: 10, color: "#333" },
              mpa: { color: "#d32f2f", fontWeight: 700, fontSize: 10 },
            },
          },
          splitLine: { show: true },
        },

        yAxis3D: {
          type: "category",
          name: this.yLabel,
          nameGap: 20,
          nameTextStyle: { fontSize: 16 },

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
              bin: { fontWeight: 600, fontSize: 10, color: "#333" },
              mpa: { color: "#d32f2f", fontWeight: 700, fontSize: 10 },
            },
          },
          splitLine: { show: true },
        },

        zAxis3D: {
          name: this.zLabel,
          nameGap: 20,
        },

        /**
         * 3D scene configuration:
         * - box sizes tune the aspect of the rendered cube
         * - viewControl sets the default camera angle/distance
         * - lighting improves depth perception
         */
        grid3D: {
          boxWidth: 50,
          boxDepth: 50,
          boxHeight: 50,

          // Leave room at bottom for labels/visualMap
          top: -30,
          bottom: 200,

          viewControl: {
            alpha: 10,
            beta: 45,
            distance: 105,
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
