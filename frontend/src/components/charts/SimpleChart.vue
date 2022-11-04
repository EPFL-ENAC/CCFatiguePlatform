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
import { colorPalette } from "@/utils/style";
import { LineChart } from "echarts/charts";
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
    aspectRatio: {
      type: Number,
      default: 1,
    },
    series: {
      type: Array,
    },
    title: {
      type: String,
    },
    xAxisName: {
      type: String,
    },
    yAxisName: {
      type: String,
    },
    xAxisType: {
      type: String,
    },
    color: {
      type: Array,
      default: () => colorPalette,
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
    actualOption: function () {
      return {
        title: {
          text: this.title,
        },
        legend: {
          type: "scroll",
        },
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
          min: "dataMin",
          max: "dataMax",
          axisLabel: {
            formatter: function (value) {
              return value.toLocaleString(undefined, {
                minimumFractionDigits: 0,
                maximumFractionDigits: 4,
              });
            },
            hideOverlap: true,
          },
        },
        yAxis: {
          name: this.yAxisName,
          nameLocation: "middle",
          nameGap: 50,
          min: "dataMin",
          max: "dataMax",
          axisLabel: {
            formatter: function (value) {
              return value.toLocaleString(undefined, {
                minimumFractionDigits: 0,
                maximumFractionDigits: 4,
              });
            },
            hideOverlap: true,
          },
        },
        tooltip: {
          trigger: "axis",
          confine: true,
          valueFormatter: (value) => value?.toFixed(2),
        },
        series: this.series.map((serie) => merge(serie, { showSymbol: false })),
        color: this.color,
      };
    },
  },
};
</script>
