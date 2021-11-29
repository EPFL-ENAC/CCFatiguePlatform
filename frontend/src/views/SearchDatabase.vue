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
            <v-card-title>Filter by text</v-card-title>
            <v-card-text>
              <v-row>
                <v-col>
                  <v-text-field
                    v-model="filters.textSearch"
                    placeholder="search"
                    dense
                  >
                  </v-text-field>
                </v-col>
              </v-row>
            </v-card-text>
          </v-card>
        </v-col>
        <v-col cols="8">
          <v-data-table
            v-model="experimentSelected"
            :headers="headers"
            :items="visibleExperiments"
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
        textSearch: "",
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
            Resin: "Tripleresin\u00ae CR58",
            Hardener: "Sika CH83-2",
            "Mixing ratio": "10:3",
          },
          Geometry: {
            Length: 200,
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
            Resin: "Tripleresin\u00ae CR58",
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
            "Curing Temperature": 26,
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
            "Curing Time": 10,
            "Curing Temperature": 22,
            "Curing Pressure": 0.95,
            "Fiber Content": 0.65,
            "Stacking Sequence": "[+-90]2s",
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
  computed: {
    possibleValues() {
      let fractureModes = [
        "All modes",
        "Mode I",
        "Mode II",
        "Mode III",
        "Combined",
      ];
      let resins = ["All resins"];
      let fiberMaterials = ["All materials"];
      let stackingSequences = ["All stacking sequences"];
      this.experiments.forEach((exp) => {
        let addIfNotExist = (array, item) => {
          if (array.indexOf(item) === -1) {
            array.push(item);
          }
        };
        addIfNotExist(resins, exp["Material Type"].Resin);
        addIfNotExist(fiberMaterials, exp["Material Type"]["Fiber Material"]);
        addIfNotExist(
          stackingSequences,
          exp["Laminates and Assemblies"]["Stacking Sequence"]
        );
      });
      return {
        fractureModes,
        resins,
        fiberMaterials,
        stackingSequences,
      };
    },
    visibleExperiments() {
      return this.experiments
        .filter((exp) => {
          return (
            (this.filters.typeFA && exp.General["Experiment Type"] == "FA") ||
            (this.filters.typeQS && exp.General["Experiment Type"] == "QS")
          );
        })
        .filter((exp) => {
          return (
            (this.filters.withFracture && exp.General.Fracture) ||
            (this.filters.withoutFracture && !exp.General.Fracture)
          );
        })
        .filter((exp) => {
          return (
            this.filters.fractureMode == "All modes" ||
            exp.General["Fracture Mode"] == this.filters.fractureMode
          );
        })
        .filter((exp) => {
          return (
            this.filters.resin == "All resins" ||
            exp["Material Type"].Resin == this.filters.resin
          );
        })
        .filter((exp) => {
          return (
            this.filters.fiberMaterial == "All materials" ||
            exp["Material Type"]["Fiber Material"] == this.filters.fiberMaterial
          );
        })
        .filter((exp) => {
          return (
            this.filters.stackingSequence == "All stacking sequences" ||
            exp["Laminates and Assemblies"]["Stacking Sequence"] ==
              this.filters.stackingSequence
          );
        })
        .filter((exp) => {
          let textSearch = this.filters.textSearch.toUpperCase();
          return (
            this.filters.textSearch == "" ||
            exp.General.Laboratory.toUpperCase().includes(textSearch) ||
            exp.General["Experiment Type"].toUpperCase().includes(textSearch) ||
            exp.Geometry.Length.toString().toUpperCase().includes(textSearch) ||
            exp.Geometry.Width.toString().toUpperCase().includes(textSearch) ||
            exp.Geometry.Thickness.toString()
              .toUpperCase()
              .includes(textSearch) ||
            exp["Laminates and Assemblies"]["Curing Time"]
              .toString()
              .toUpperCase()
              .includes(textSearch) ||
            exp["Laminates and Assemblies"]["Curing Temperature"]
              .toString()
              .toUpperCase()
              .includes(textSearch) ||
            exp["Laminates and Assemblies"]["Curing Pressure"]
              .toString()
              .toUpperCase()
              .includes(textSearch) ||
            exp["Laminates and Assemblies"]["Fiber Content"]
              .toString()
              .toUpperCase()
              .includes(textSearch) ||
            exp["Laminates and Assemblies"]["Stacking Sequence"]
              .toUpperCase()
              .includes(textSearch) ||
            exp["Laminates and Assemblies"]["Curing Time"]
              .toString()
              .toUpperCase()
              .includes(textSearch)
          );
        });
    },
  },
};
</script>

<style scoped lang="scss"></style>
