<template>
  <v-container>
    <v-card elevation="0">
      <v-card-title> Search Experiments Database </v-card-title>
      <v-data-table :headers="headers" :items="experiments"> </v-data-table>
    </v-card>

    <v-container>
      <v-row justify="center">
        <v-btn class="ma-2">Download raw files</v-btn>
        <v-btn class="ma-2" :to="{ name: 'TestSelection' }"
          >View tests results</v-btn
        >
        <v-btn class="ma-2" :to="{ name: 'CCFatigueAnalysis' }"
          >Analyse experiment</v-btn
        >
      </v-row>
    </v-container>
  </v-container>
</template>

<script>
export default {
  name: "SearchDatabase",
  data() {
    return {
      headers: [
        { text: "Laboratory", value: "General.Laboratory" },
        { text: "Experiment Type", value: "General.Experiment Type" },
        { text: "Publication", value: "Publication.DOI" },
        { text: "Images", value: "Publication.Images Repository" },
        { text: "Length", value: "Geometry.Length" },
        { text: "width", value: "Geometry.Width" },
        { text: "Thickness", value: "Geometry.Thickness" },
        { text: "Curing Time", value: "Laminates and Assemblies.Curing Time" },
        {
          text: "Curing Temperature",
          value: "Laminates and Assemblies.Curing Temperature",
        },
        {
          text: "Curing Pressure",
          value: "Laminates and Assemblies.Curing Pressure",
        },
        {
          text: "Fiber Content",
          value: "Laminates and Assemblies.Fiber Content",
        },
        {
          text: "Stacking Sequence",
          value: "Laminates and Assemblies.Stacking Sequence",
        },
      ],
      experiments: [
        {
          General: {
            Laboratory: "CCLAB",
            Researcher: "AAA FakeResearcher",
            "Experiment Type": "QS",
            Date: "2019-09",
            Fracture: false,
            "Fracture Mode": null,
            "Initial Crack length": null,
            "Fatigue Test Type": null,
            "Measuring Equipment": "Instron Machine",
            "Reliability Level": null,
            "Control mode": "Load Controlled",
          },
          Publication: {
            Title: null,
            Author: null,
            Year: null,
            DOI: null,
            "Images Repository": null,
          },
          "Material Type": {
            "Fiber Material": "E-glass fiber fabrics (EC 9-68)",
            "Fiber Geometry": null,
            "Area Density": 425,
            Resin: "Biresin\u00ae CR83",
            Hardener: "Sika CH83-2",
            "Mixing ratio": "10:3",
          },
          Geometry: {
            Length: 160,
            Width: 25.1,
            Thickness: 2.85,
          },
          "Laminates and Assemblies": {
            "Curing Time": 8,
            "Curing Temperature": 22,
            "Curing Pressure": 0.95,
            "Fiber Content": 0.65,
            "Stacking Sequence": "[+-45]2s",
          },
          "Test condtions": {
            Temperature: 22,
            Humidity: 40,
          },
          "DIC Analysis ": {
            "Subset Size": null,
            "Step Size": null,
          },
        },
        {
          General: {
            Laboratory: "CCLAB",
            Researcher: "Shayan Khalooei",
            "Experiment Type": "FA",
            Date: "2021-06",
            Fracture: false,
            "Fracture Mode": null,
            "Initial Crack length": null,
            "Fatigue Test Type": "CA",
            "Measuring Equipment": "Instron Machine",
            "Reliability Level": null,
            "Control mode": "Load Controlled",
          },
          Publication: {
            Title: null,
            Author: null,
            Year: null,
            DOI: null,
            "Images Repository": null,
          },
          "Material Type": {
            "Fiber Material": "E-glass fiber fabrics (EC 9-68)",
            "Fiber Geometry": null,
            "Area Density": 425,
            Resin: "Biresin\u00ae CR83",
            Hardener: "Sika CH83-2",
            "Mixing ratio": "10:3",
          },
          Geometry: {
            Length: 160,
            Width: 25.1,
            Thickness: 2.85,
          },
          "Laminates and Assemblies": {
            "Curing Time": 8,
            "Curing Temperature": 22,
            "Curing Pressure": 0.95,
            "Fiber Content": 0.65,
            "Stacking Sequence": "[+-45]2s",
          },
          "Test condtions": {
            Temperature: 22,
            Humidity: 40,
          },
          "DIC Analysis ": {
            "Subset Size": null,
            "Step Size": null,
          },
        },
        {
          General: {
            Laboratory: "CCLAB",
            Researcher: "Shayan Khalooei",
            "Experiment Type": "FA",
            Date: "2021-06",
            Fracture: false,
            "Fracture Mode": null,
            "Initial Crack length": null,
            "Fatigue Test Type": "CA",
            "Measuring Equipment": "Instron Machine",
            "Reliability Level": null,
            "Control mode": "Load Controlled",
          },
          Publication: {
            Title: null,
            Author: null,
            Year: null,
            DOI: null,
            "Images Repository": null,
          },
          "Material Type": {
            "Fiber Material": "E-glass fiber fabrics (EC 9-68)",
            "Fiber Geometry": null,
            "Area Density": 425,
            Resin: "Biresin\u00ae CR83",
            Hardener: "Sika CH83-2",
            "Mixing ratio": "10:3",
          },
          Geometry: {
            Length: 160,
            Width: 25.1,
            Thickness: 2.85,
          },
          "Laminates and Assemblies": {
            "Curing Time": 8,
            "Curing Temperature": 22,
            "Curing Pressure": 0.95,
            "Fiber Content": 0.65,
            "Stacking Sequence": "[+-45]2s",
          },
          "Test condtions": {
            Temperature: 22,
            Humidity: 40,
          },
          "DIC Analysis ": {
            "Subset Size": null,
            "Step Size": null,
          },
        },
        {
          General: {
            Laboratory: "CCLAB",
            Researcher: "Shayan Khalooei",
            Date: "2021-06",
            "Experiment Type": "FA",
            Fracture: false,
            "Fracture Mode": null,
            "Initial Crack length": null,
            "Fatigue Test Type": "CA",
            "Measuring Equipment": "Instron Machine",
            "Reliability Level": null,
            "Control mode": "Load Controlled",
          },
          Publication: {
            Title: null,
            Author: null,
            Year: null,
            DOI: null,
            "Images Repository": null,
          },
          "Material Type": {
            "Fiber Material": "E-glass fiber fabrics (EC 9-68)",
            "Fiber Geometry": null,
            "Area Density": 425,
            Resin: "Biresin\u00ae CR83",
            Hardener: "Sika CH83-2",
            "Mixing ratio": "10:3",
          },
          Geometry: {
            Length: 160,
            Width: 25.1,
            Thickness: 2.85,
          },
          "Laminates and Assemblies": {
            "Curing Time": 8,
            "Curing Temperature": 22,
            "Curing Pressure": 0.95,
            "Fiber Content": 0.65,
            "Stacking Sequence": "[+-45]2s",
          },
          "Test condtions": {
            Temperature: 22,
            Humidity: 40,
          },
          "DIC Analysis ": {
            "Subset Size": null,
            "Step Size": null,
          },
        },
      ],
    };
  },
};
</script>

<style scoped lang="scss">
</style>
