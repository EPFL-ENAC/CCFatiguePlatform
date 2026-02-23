<template>
  <div ref="chart" style="width: 100%; height: 520px" />
</template>

<script>
import * as echarts from "echarts";
import "echarts-gl";

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
  mounted() {
    this.chart = echarts.init(this.$refs.chart);
    this.render();
    window.addEventListener("resize", this.onResize);
  },
  beforeUnmount() {
    window.removeEventListener("resize", this.onResize);
    if (this.chart) this.chart.dispose();
  },
  watch: {
    data: {
      deep: true,
      handler() {
        this.render();
      },
    },
  },
  methods: {
    onResize() {
      if (this.chart) this.chart.resize();
    },
    render() {
      if (!this.chart) return;

      const hasCenters =
        this.xCenters &&
        this.yCenters &&
        this.xCenters.length &&
        this.yCenters.length;

      const option = {
        title: { text: this.title, left: "center" },
        tooltip: {
          formatter: (p) => {
            const [i, j, z] = p.value;
            const xb = hasCenters ? this.xCenters[i] : i;
            const yb = hasCenters ? this.yCenters[j] : j;

            return [
              `${this.xLabel}: <b>${
                hasCenters ? xb.toFixed(3) : xb
              }</b> (bin ${i})`,
              `${this.yLabel}: <b>${
                hasCenters ? yb.toFixed(3) : yb
              }</b> (bin ${j})`,
              `${this.zLabel}: <b>${Number(z).toFixed(3)}</b>`,
            ].join("<br/>");
          },
        },
        visualMap: {
          show: true,
          dimension: 2,
          calculable: true,
          left: 10,
          bottom: 10,
        },
        xAxis3D: { type: "category", name: this.xLabel },
        yAxis3D: { type: "category", name: this.yLabel },
        zAxis3D: { type: "value", name: this.zLabel },
        grid3D: {
          boxWidth: 120,
          boxDepth: 120,
          viewControl: {
            projection: "perspective",
            // tu peux ajuster ces 2 paramètres si besoin
            rotateSensitivity: 1,
            zoomSensitivity: 1,
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
            data: this.data,
            barSize: 0.95,
          },
        ],
      };

      this.chart.setOption(option, true);
    },
  },
};
</script>
