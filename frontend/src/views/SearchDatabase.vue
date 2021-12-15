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
                    @change="fetchExperiments"
                  />
                </v-col>
                <v-col>
                  <v-checkbox
                    v-model="filters.typeQS"
                    label="QS"
                    hide-details
                    @change="fetchExperiments"
                  />
                </v-col>
              </v-row>
              <v-row>
                <v-col>
                  <v-checkbox
                    v-model="filters.withFracture"
                    label="with fracture"
                    hide-details
                    @change="fetchExperiments"
                  />
                </v-col>
                <v-col>
                  <v-checkbox
                    v-model="filters.withoutFracture"
                    label="without fracture"
                    hide-details
                    @change="fetchExperiments"
                  />
                </v-col>
              </v-row>
              <v-row v-if="filters.withFracture && !filters.withoutFracture">
                <v-col>
                  <v-overflow-btn
                    v-model="filters.fractureMode"
                    :items="possibleValues.fractureModes"
                    label="fracture mode"
                    hide-details
                    dense
                    @change="fetchExperiments"
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
                    @change="fetchExperiments"
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
                    @change="fetchExperiments"
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
                    @change="fetchExperiments"
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
                    @change="fetchExperiments"
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
            :items="experiments"
            :options.sync="paginationOptions"
            :server-items-length="nbExperiments"
            :loading="loading"
            :footer-props="{
              'items-per-page-options': [5, 10, 15, 20, 40],
            }"
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
import { mapState } from "vuex";

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
        { text: "Laboratory", value: "laboratory" },
        { text: "Experiment Type", value: "experiment_type" },
        { text: "Publication", value: "publication_doi" },
        { text: "Images", value: "publication_images_repository" },
        { text: "Length", value: "geometry_length" },
        { text: "width", value: "geometry_width" },
        { text: "Thickness", value: "geometry_thickness" },
        { text: "Curing Time", value: "laminates_and_assemblies_curing_time" },
        {
          text: "Curing Temperature",
          value: "laminates_and_assemblies_curing_temperature",
        },
        {
          text: "Curing Pressure",
          value: "laminates_and_assemblies_curing_pressure",
        },
        {
          text: "Fiber Content",
          value: "laminates_and_assemblies_fiber_content",
        },
        {
          text: "Stacking Sequence",
          value: "laminates_and_assemblies_stacking_sequence",
        },
      ],
      experimentSelected: [],
      paginationOptions: {},
    };
  },
  computed: {
    ...mapState("experiments", {
      loading: "loading",
      experiments: "experiments",
      page: "page",
      itemsPerPage: "itemsPerPage",
      nbExperiments: "nbExperiments",
    }),
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
        addIfNotExist(resins, exp.material_type_resin);
        addIfNotExist(fiberMaterials, exp.material_type_fiber_material);
        addIfNotExist(
          stackingSequences,
          exp.laminates_and_assemblies_stacking_sequence
        );
      });
      return {
        fractureModes,
        resins,
        fiberMaterials,
        stackingSequences,
      };
    },
  },
  created() {
    this.fetchExperiments();
  },
  watch: {
    paginationOptions: {
      handler() {
        this.fetchExperiments();
      },
      deep: true,
    },
  },
  methods: {
    fetchExperiments() {
      let queryElements = [];
      let types = [];
      let filterToEmpty = false;

      // type (FA|QS)
      if (this.filters.typeFA) {
        types.push("FA");
      }
      if (this.filters.typeQS) {
        types.push("QS");
      }
      queryElements.push("experiment_type:" + types.join(","));

      // fracture (true|false)
      // fracture_mode (All modes|Mode I|Mode II|Mode III|Combined)
      if (this.filters.withFracture && !this.filters.withoutFracture) {
        queryElements.push("fracture:1");
        if (this.filters.fractureMode !== "All modes") {
          queryElements.push(`fracture_mode:${this.filters.fractureMode}`);
        }
      } else if (!this.filters.withFracture && this.filters.withoutFracture) {
        queryElements.push("fracture:0");
      } else if (!this.filters.withFracture && !this.filters.withoutFracture) {
        filterToEmpty = true;
      }

      // material_type_resin
      if (this.filters.resin !== "All resins") {
        queryElements.push(`material_type_resin:${this.filters.resin}`);
      }

      // material_type_fiber_material
      if (this.filters.fiberMaterial !== "All materials") {
        queryElements.push(
          `material_type_fiber_material:${this.filters.fiberMaterial}`
        );
      }

      // laminates_and_assemblies_stacking_sequence
      if (this.filters.stackingSequence !== "All stacking sequences") {
        queryElements.push(
          `laminates_and_assemblies_stacking_sequence:${this.filters.stackingSequence}`
        );
      }

      this.$store.dispatch("experiments/fetch", {
        query: queryElements.join(";"),
        textSearch: this.filters.textSearch,
        paginationOptions: this.paginationOptions,
        filterToEmpty,
      });
    },
  },
};
</script>

<style scoped lang="scss"></style>
