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
                    :items="[fractureModeAll, ...allFractureMode]"
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
                    :items="[resinsAll, ...allMaterialTypeResin]"
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
                    :items="[
                      fiberMaterialsAll,
                      ...allMaterialTypeFiberMaterial,
                    ]"
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
                    :items="[
                      stackingSequencesAll,
                      ...allLaminatesAndAssembliesStackingSequence,
                    ]"
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
            :items="filteredExperiments.experiments"
            :options.sync="options"
            :server-items-length="filteredExperiments.pagination.total"
            :loading="filteredExperiments.loading"
            :footer-props="{
              'items-per-page-options': [5, 10, 15, 20, 40],
            }"
            item-key="id"
            @click:row="rowClick"
            single-select
          >
            <template v-slot:no-data>No experiment in database</template>
          </v-data-table>
          <v-container>
            <v-row justify="center">
              <v-tooltip bottom>
                <template v-slot:activator="{ on, attrs }">
                  <v-btn
                    class="ma-2"
                    :disabled="experimentSelected.length === 0"
                    @click="downloadRawFiles"
                    v-bind="attrs"
                    v-on="on"
                  >
                    Download raw files
                  </v-btn>
                </template>
                <span>- not implemented yet -</span>
              </v-tooltip>
              <v-btn
                class="ma-2"
                :disabled="experimentSelected.length === 0"
                @click="viewTestsSelection"
              >
                View tests results
              </v-btn>
              <v-tooltip bottom>
                <template v-slot:activator="{ on, attrs }">
                  <v-btn
                    class="ma-2"
                    :disabled="experimentSelected.length === 0"
                    @click="analyseExperiment"
                    v-bind="attrs"
                    v-on="on"
                  >
                    Analyse experiment
                  </v-btn>
                </template>
                <span>- not implemented yet -</span>
              </v-tooltip>
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
      fractureModeAll: "All fracture modes",
      fiberMaterialsAll: "All materials",
      resinsAll: "All resins",
      stackingSequencesAll: "All stacking sequences",

      filters: {
        typeFA: true,
        typeQS: true,
        withFracture: true,
        withoutFracture: true,
        fractureMode: "All fracture modes",
        fiberMaterial: "All materials",
        resin: "All resins",
        stackingSequence: "All stacking sequences",
        textSearch: "",
      },

      options: {
        page: 1,
        itemsPerPage: 10,
      },

      headers: [
        { text: "Laboratory", value: "laboratory" },
        { text: "Experiment Type", value: "experiment_type" },
        { text: "Publication", value: "publication_doi" },
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
          text: "Stacking Sequence",
          value: "laminates_and_assemblies_stacking_sequence",
        },
      ],
      experimentSelected: [],
    };
  },
  computed: {
    ...mapState("experiments", {
      filteredExperiments: "filteredExperiments",
      allFractureMode: "allFractureMode",
      allMaterialTypeFiberMaterial: "allMaterialTypeFiberMaterial",
      allMaterialTypeResin: "allMaterialTypeResin",
      allLaminatesAndAssembliesStackingSequence:
        "allLaminatesAndAssembliesStackingSequence",
    }),
  },
  created() {
    this.$store.dispatch("experiments/fetchAllFiltersValues");
    this.fetchExperiments();
  },
  watch: {
    options: {
      handler() {
        this.fetchExperiments();
      },
      deep: true,
    },
  },
  methods: {
    rowClick(_item, row) {
      row.select(!row.isSelected);
    },
    fetchExperiments() {
      this.$store.dispatch("experiments/fetchFilteredExperiments", {
        filters: {
          typeFA: this.filters.typeFA,
          typeQS: this.filters.typeQS,
          withFracture: this.filters.withFracture,
          withoutFracture: this.filters.withoutFracture,
          fractureMode:
            this.filters.withFracture &&
            !this.filters.withoutFracture &&
            this.filters.fractureMode !== this.fractureModeAll
              ? this.filters.fractureMode
              : null,
          fiberMaterial:
            this.filters.fiberMaterial !== this.fiberMaterialsAll
              ? this.filters.fiberMaterial
              : null,
          resin:
            this.filters.resin !== this.resinsAll ? this.filters.resin : null,
          stackingSequence:
            this.filters.stackingSequence !== this.stackingSequencesAll
              ? this.filters.stackingSequence
              : null,
          textSearch: this.filters.textSearch,
        },
        pagination: {
          page: this.options.page,
          size: this.options.itemsPerPage,
        },
      });
    },
    downloadRawFiles() {
      console.log(
        `Download Raw files for experiment ${this.experimentSelected[0].id} : not implemented yet.`
      );
    },
    viewTestsSelection() {
      this.$router.push({
        name: "TestsSelection",
        query: { exp: this.experimentSelected[0].id },
      });
    },
    analyseExperiment() {
      console.log(
        `Analyse experiment ${this.experimentSelected[0].id} : not implemented yet.`
      );
    },
  },
};
</script>

<style scoped lang="scss"></style>
