<template>
  <v-container>
    <v-card flat>
      <v-card-title>Search Experiments Database</v-card-title>
      <v-row>
        <v-col>
          <v-card flat>
            <v-card-title>Experiment type</v-card-title>
            <v-card-text>
              <v-row>
                <v-col>
                  <v-checkbox
                    v-model="filters.typeFA"
                    label="FA"
                    hide-details
                  />
                </v-col>
                <v-col>
                  <v-checkbox
                    v-model="filters.typeQS"
                    label="QS"
                    hide-details
                  />
                </v-col>
              </v-row>
              <v-row>
                <v-col>
                  <v-checkbox
                    v-model="filters.withFracture"
                    label="with fracture"
                    hide-details
                  />
                </v-col>
                <v-col>
                  <v-checkbox
                    v-model="filters.withoutFracture"
                    label="without fracture"
                    hide-details
                  />
                </v-col>
              </v-row>
              <v-row v-if="filters.withFracture">
                <v-col>
                  <v-overflow-btn
                    v-model="filters.fractureMode"
                    :items="possibleValues.fractureModes"
                    label="fracture mode"
                    hide-details
                    dense
                  >
                  </v-overflow-btn>
                </v-col>
              </v-row>
            </v-card-text>
          </v-card>
          <v-card flat>
            <v-card-title>Material type</v-card-title>
            <v-card-text>
              <v-row>
                <v-col>
                  <v-overflow-btn
                    v-model="filters.resin"
                    :items="possibleValues.resins"
                    hide-details
                    dense
                  >
                  </v-overflow-btn>
                </v-col>
              </v-row>
              <v-row>
                <v-col>
                  <v-overflow-btn
                    v-model="filters.fiberMaterial"
                    :items="possibleValues.fiberMaterials"
                    hide-details
                    dense
                  >
                  </v-overflow-btn>
                </v-col>
              </v-row>
            </v-card-text>
          </v-card>
          <v-card flat>
            <v-card-title>Laminates &amp; Assemblies</v-card-title>
            <v-card-text>
              <v-row>
                <v-col>
                  <v-overflow-btn
                    v-model="filters.stackingSequence"
                    :items="possibleValues.stackingSequences"
                    hide-details
                    dense
                  >
                  </v-overflow-btn>
                </v-col>
              </v-row>
            </v-card-text>
          </v-card>
          <v-card flat>
            <v-card-title>Loading information</v-card-title>
            <v-card-text>
              <v-row>
                <v-col> todo </v-col>
              </v-row>
            </v-card-text>
          </v-card>
        </v-col>
        <v-col cols="8">
          <v-data-table
            v-model="experimentSelected"
            :headers="headers"
            :items="experiments"
            item-key="id"
            show-select
            single-select
          >
          </v-data-table>
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
        </v-col>
      </v-row>
    </v-card>
  </v-container>
</template>

<script>
export default {
  name: "SearchDatabase",
  data() {
    return {
      filters: {
        typeFA: true,
        typeQS: true,
        withFracture: true,
        withoutFracture: true,
        fractureMode: "All modes",
        resin: "All resins",
        fiberMaterial: "All materials",
        stackingSequence: "All stacking sequences",
      },
      possibleValues: {
        fractureModes: [
          "All modes",
          "Mode I",
          "Mode II",
          "Mode III",
          "Combined",
        ],
        resins: ["All resins", "Biresin\u00ae CR83"],
        fiberMaterials: ["All materials", "E-glass fiber fabrics (EC 9-68)"],
        stackingSequences: ["All stacking sequences", "[+-45]2s"],
      },
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
          id: 0,
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
          id: 1,
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
          id: 2,
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
          id: 3,
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
      experimentSelected: [],
    };
  },
};
</script>

<style scoped lang="scss"></style>
