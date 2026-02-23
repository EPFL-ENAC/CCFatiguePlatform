<template>
  <div ref="chart" style="width: 100%; height: 800px" />
</template>

<script>
import * as echarts from "echarts";

export default {
  name: "Markov3D",
  props: {
    // data format: Array of [i, j, z]
    data: { type: Array, required: true },
    xLabel: { type: String, default: "Range bin" },
    yLabel: { type: String, default: "Mean bin" },
    zLabel: { type: String, default: "Cycles" },
    // optional: bin centers for tooltip
    xCenters: { type: Array, default: () => [] }, // length nx
    yCenters: { type: Array, default: () => [] }, // length ny
    title: { type: String, default: "Markov matrix - 3D histogram" },
  },
  data() {
    return { chart: null };
  },
  watch: {
    data: {
      deep: true,
      handler() {
        this.render();
      },
    },
  },

  mounted() {
    this.chart = echarts.init(this.$refs.chart);
    this.render();
    window.addEventListener("resize", this.onResize);
  },

  beforeDestroy() {
    window.removeEventListener("resize", this.onResize);
    if (this.chart) this.chart.dispose();
  },
  methods: {
    onResize() {
      if (this.chart) this.chart.resize();
    },
    render() {
      if (!this.chart) return;

      const data = Array.isArray(this.data) ? this.data : [];

      const zs = data.map((d) => d[2]).filter(Number.isFinite);
      const zMin = zs.length ? Math.min(...zs) : 0;
      const zMax = zs.length ? Math.max(...zs) : 1;

      const fmt = (v) => (Number.isFinite(v) ? Number(v).toFixed(1) : "-");
      const xCount = this.xCenters?.length || 64;
      const yCount = this.yCenters?.length || 64;

      const option = {
        // si tu veux pas le titre echarts dans le graphe (tu utilises ton titre HTML),
        // mets title: { show: false }
        title: { show: false },

        tooltip: {
          formatter: (p) => {
            const v = p.value || [];
            const i = v[0];
            const j = v[1];
            const z = v[2];

            // centres MPa (si tu ne les passes pas, on fallback)
            const xPhys = Number.isFinite(this.xCenters?.[i])
              ? this.xCenters[i]
              : v[3];
            const yPhys = Number.isFinite(this.yCenters?.[j])
              ? this.yCenters[j]
              : v[4];

            const cPhys = "#d32f2f";
            const cBin = "#666";

            return [
              `${
                this.xLabel
              }: <span style="color:${cPhys};font-weight:700">${fmt(
                xPhys
              )} MPa</span> <span style="color:${cBin}">(bin ${i})</span>`,
              `${
                this.yLabel
              }: <span style="color:${cPhys};font-weight:700">${fmt(
                yPhys
              )} MPa</span> <span style="color:${cBin}">(bin ${j})</span>`,
              `${this.zLabel}: <b>${fmt(z)}</b>`,
            ].join("<br/>");
          },
        },

        visualMap: {
          show: true,
          dimension: 2,
          calculable: true,
          left: 10,
          bottom: 10,
          min: zMin,
          max: zMax,
        },

        xAxis3D: {
          type: "category",
          name: this.xLabel,
          nameGap: 30,
          nameTextStyle: { fontSize: 14, fontWeight: 700 },
          data: Array.from({ length: xCount }, (_, i) => i),
          axisLabel: {
            interval: "auto",
            margin: 14,
            fontSize: 12,
            lineHeight: 16,
            rotate: 15,
            formatter: (i) => {
              const mp = this.xCenters?.[i];
              return `{bin|${i}} {mpa|(${fmt(mp)} MPa)}`;
            },
            rich: {
              bin: { fontWeight: 600 },
              mpa: { color: "#d32f2f", fontWeight: 700 },
            },
          },
        },

        yAxis3D: {
          type: "category",
          name: this.yLabel,
          nameGap: 30,
          nameTextStyle: { fontSize: 14, fontWeight: 700 },
          data: Array.from({ length: yCount }, (_, j) => j),
          axisLabel: {
            interval: "auto",
            margin: 14,
            fontSize: 12,
            lineHeight: 16,
            rotate: 15,
            formatter: (j) => {
              const mp = this.yCenters?.[j];
              return `{bin|${j}} {mpa|(${fmt(mp)} MPa)}`;
            },
            rich: {
              bin: { fontWeight: 600 },
              mpa: { color: "#d32f2f", fontWeight: 700 },
            },
          },
        },

        zAxis3D: {
          name: this.zLabel,
          nameGap: 20,
          nameTextStyle: { fontSize: 14, fontWeight: 700 },
        },
        grid3D: {
          boxWidth: 140,
          boxDepth: 140,
          boxHeight: 110,
          viewControl: {
            alpha: 10,
            beta: 44,
            distance: 230,
          },
          light: {
            main: { intensity: 1.2 },
            ambient: { intensity: 0.4 },
          },
        },

        // ✅ UNE SEULE clé series
        series: [
          {
            type: "bar3D",
            shading: "lambert",
            data: data.map((d) => [d[0], d[1], d[2]]), // i, j, z
            barSize: 0.95,
          },
        ],
      };

      this.chart.setOption(option, true);
    },
  },
};
</script>
