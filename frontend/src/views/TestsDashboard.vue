<template>
  <v-container>
    <v-row>
      <v-col>
        <experiment-specifications :experiment="experiment.experiment" />
      </v-col>
    </v-row>

    <v-row>
      <v-col cols="auto">
        <h2>Test results</h2>
      </v-col>
      <v-spacer />
      <v-col cols="auto">
        <v-btn @click="goBack">Add test(s)</v-btn>
      </v-col>
    </v-row>

    <!-- Fatigue branch -->
    <v-row v-if="experimentType === 'FA' && !isFracture">
      <v-col cols="10">
        <v-row>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                <v-row align="center" class="w-100">
                  <v-col class="d-flex align-center" cols="auto">
                    <span>Hysteresis Loops</span>
                    <info-tooltip>
                      Ten hysteresis loops selected at intervals corresponding
                      to one-tenth of the specimen’s fatigue life.
                    </info-tooltip>
                  </v-col>
                  <v-spacer />
                  <v-col cols="auto">
                    <v-select
                      v-model="selectedLoopIndices"
                      :items="loopIndexOptions"
                      label="Cycle index"
                      dense
                      hide-details
                      multiple
                      style="max-width: 220px"
                    >
                      <template #prepend-item>
                        <v-list-item @click="selectedLoopIndices = []">
                          <v-list-item-title class="text-primary"
                            >Clear all</v-list-item-title
                          >
                        </v-list-item>
                        <v-divider />
                      </template>
                    </v-select>
                  </v-col>
                </v-row>
              </v-card-title>
              <v-card-text>
                <simple-chart
                  :series="strainStressSeriesFA"
                  :aspect-ratio="2"
                  x-axis-name="Strain [-]"
                  y-axis-name="Stress [MPa]"
                  :axis-label-formatter="axisTickFormatter"
                />
              </v-card-text>
            </v-card>
          </v-col>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                <v-row align="center" class="w-100">
                  <v-col class="d-flex align-center" cols="auto">
                    <span>Creep evolution</span>
                    <info-tooltip>
                      Creep is defined as the average deformation during each
                      cycle, it is calculated as the center of the hysteresis
                      loop ellipse
                    </info-tooltip>
                  </v-col>
                  <v-spacer />
                  <v-col cols="auto" class="d-flex">
                    <v-select
                      v-model="xAxisMode"
                      :items="[
                        { text: 'Cycle count', value: 'normal' },
                        { text: 'Log(Cycle count)', value: 'log' },
                        { text: 'Normalized cycle count', value: 'normalized' },
                      ]"
                      dense
                      hide-details
                      label="X-Axis scale"
                      style="max-width: 220px"
                    />
                  </v-col>
                </v-row>
              </v-card-title>
              <v-card-text>
                <simple-chart
                  :series="creepSeries"
                  :aspect-ratio="2"
                  :x-axis-name="computedXAxisLabel"
                  :x-axis-type="xAxisChartType"
                  y-axis-name="Creep [-]"
                  :x-axis-min="xAxisMode === 'normalized' ? 0 : null"
                  :x-axis-max="xAxisMode === 'normalized' ? 1 : null"
                  :axis-label-formatter="axisTickFormatter"
                  :tooltip-formatter="TooltipFormatter_5"
                />
              </v-card-text>
            </v-card>
          </v-col>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                <v-row align="center" class="w-100">
                  <v-col class="d-flex align-center" cols="auto">
                    <span>Hysteresis area evolution</span>
                    <info-tooltip
                      >The hysteresis area, i.e., the area enclosed by the
                      hysteresis loop, evolution throughout specimens' fatigue
                      lives</info-tooltip
                    >
                  </v-col>
                  <v-spacer />
                  <v-col cols="auto" class="d-flex">
                    <v-select
                      v-model="xAxisMode"
                      :items="[
                        { text: 'Cycle count', value: 'normal' },
                        { text: 'Log(Cycle count)', value: 'log' },
                        { text: 'Normalized cycle count', value: 'normalized' },
                      ]"
                      dense
                      hide-details
                      label="X-Axis scale"
                      style="max-width: 220px"
                    />
                  </v-col>
                </v-row>
              </v-card-title>
              <v-card-text>
                <simple-chart
                  :series="hysteresisAreaSeries"
                  :aspect-ratio="2"
                  :x-axis-name="computedXAxisLabel"
                  :x-axis-type="xAxisChartType"
                  y-axis-name="Hysteresis area [MPa]"
                  :x-axis-min="xAxisMode === 'normalized' ? 0 : null"
                  :x-axis-max="xAxisMode === 'normalized' ? 1 : null"
                  :axis-label-formatter="axisTickFormatter"
                  :tooltip-formatter="TooltipFormatter_5"
                />
              </v-card-text>
            </v-card>
          </v-col>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                Stiffness evolution under cyclic loading
                <info-tooltip>
                  Stiffness is calculated as the slope of the linear fit of the
                  points forming the hysteresis loop ellipse
                </info-tooltip>
              </v-card-title>
              <v-card-text>
                <v-row class="mb-6">
                  <v-col>
                    <v-select
                      v-model="xAxisMode"
                      :items="[
                        { text: 'Cycle count', value: 'normal' },
                        { text: 'Log(Cycle count)', value: 'log' },
                        { text: 'Normalized cycle count', value: 'normalized' },
                      ]"
                      dense
                      hide-details
                      label="X-Axis scale"
                    />
                  </v-col>
                  <v-col>
                    <v-select
                      v-model="yAxisStiffnessMode"
                      :items="[
                        { text: 'Absolute', value: 'absolute' },
                        { text: 'Normalized', value: 'normalized' },
                      ]"
                      dense
                      hide-details
                      label="Y-Axis scale"
                    />
                  </v-col>
                </v-row>

                <simple-chart
                  :series="stiffnessSeries"
                  :aspect-ratio="2"
                  :x-axis-name="computedXAxisLabel"
                  :x-axis-type="xAxisChartType"
                  :y-axis-name="computedYAxisStiffnessLabel"
                  :y-axis-max="yAxisMaxStiffness"
                  :x-axis-min="xAxisMode === 'normalized' ? 0 : null"
                  :x-axis-max="xAxisMode === 'normalized' ? 1 : null"
                  :axis-label-formatter="axisTickFormatter"
                />
              </v-card-text>
            </v-card>
          </v-col>
        </v-row>
      </v-col>
      <v-col cols="2">
        <v-card :loading="loading">
          <v-card-text>
            <ul>
              <li>
                <experiment-s-v
                  subject="Specimen number"
                  :values="specimenIds"
                  :colors="valueColors"
                  value-type="bigNumber"
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Stress at failure"
                  :values="formattedStressAtFailure"
                  :colors="valueColors"
                  :unit="units.stress"
                  tooltip="σ_fail is the stress level that induced failure..."
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Strain at failure"
                  :values="formattedStrainAtFailure"
                  :colors="valueColors"
                  unit="%"
                  tooltip="ε_fail is the deformation at the time of failure..."
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Cycle at failure"
                  :values="cycleAtFailure"
                  :colors="valueColors"
                  value-type="bigNumber"
                  tooltip="Number of cycles to failure. Plotted values can be up to the last DIC recorded value"
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Run out"
                  :values="runOuts"
                  :colors="valueColors"
                  tooltip="No fatigue failure."
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Total dissipated energy (TDE)"
                  :values="formattedTotalDissipatedEnergies"
                  :colors="valueColors"
                  :unit="units.stress"
                  tooltip="Sum of all hysteresis areas."
                />
              </li>
            </ul>
            <v-alert
              v-if="hasWarnings"
              type="warning"
              dense
              outlined
              class="mt-4"
            >
              Smooth spikes and drop function has been activated!
            </v-alert>
          </v-card-text>
        </v-card>
      </v-col>
    </v-row>
    <v-row v-else-if="experimentType === 'FA' && isFracture">
      <v-col cols="10">
        <v-row>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                <v-row align="center" class="w-100">
                  <v-col class="d-flex align-center" cols="auto">
                    Crack length & Load vs Number of cycles
                    <info-tooltip>
                      The graph shows the evolution of the crack length (dotted
                      lines) and load (solid line) during the test.
                    </info-tooltip>
                  </v-col>
                  <v-spacer />
                  <v-col cols="auto" class="d-flex">
                    <v-select
                      v-model="xAxisMode"
                      :items="[
                        { text: 'Cycle count', value: 'normal' },
                        { text: 'Log(Cycle count)', value: 'log' },
                        { text: 'Normalized cycle count', value: 'normalized' },
                      ]"
                      dense
                      hide-details
                      label="X-Axis scale"
                      style="max-width: 220px"
                    />
                  </v-col>
                </v-row>
              </v-card-title>
              <v-card-text>
                <double-chart
                  :series="crackFaFractureSeries"
                  :aspect-ratio="2"
                  :x-axis-name="computedXAxisLabel"
                  :x-axis-type="xAxisChartType"
                  :x-axis-max="xAxisMaxDoubleChart"
                  :x-axis-min="xAxisMinDoubleChart"
                  :y1-axis-name="'Load [kN]'"
                  :y1-axis-max="y1AxisMaxDoubleChart"
                  :y1-axis-min="y1AxisMinDoubleChart"
                  :y2-axis-name="'Crack Length [mm]'"
                  :y2-axis-max="y2AxisMaxDoubleChart"
                  :y2-axis-min="y2AxisMinDoubleChart"
                  :axis-label-formatter="axisTickFormatter"
                />
              </v-card-text>
            </v-card>
          </v-col>

          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                <v-row align="center" class="w-100">
                  <v-col class="d-flex align-center" cols="auto">
                    Fracture Energy vs Number of cycles
                    <info-tooltip>
                      Fracture energy is calculated with the Modified Beam
                      Theory.
                    </info-tooltip>
                  </v-col>
                  <v-spacer />
                  <v-col cols="auto" class="d-flex">
                    <v-select
                      v-model="xAxisMode"
                      :items="[
                        { text: 'Cycle count', value: 'normal' },
                        { text: 'Log(Cycle count)', value: 'log' },
                        { text: 'Normalized cycle count', value: 'normalized' },
                      ]"
                      dense
                      hide-details
                      label="X-Axis scale"
                      style="max-width: 220px"
                    />
                  </v-col>
                </v-row>
              </v-card-title>
              <v-card-text>
                <simple-chart
                  :series="fractureGMBTSeries"
                  :aspect-ratio="2"
                  :x-axis-name="computedXAxisLabel"
                  :x-axis-type="xAxisChartType"
                  y-axis-name="G [J/m²]"
                  :y-axis-max="yAxisMaxGMBT"
                  :x-axis-min="xAxisMode === 'normalized' ? 0 : null"
                  :x-axis-max="xAxisMode === 'normalized' ? 1 : null"
                  :axis-label-formatter="axisTickFormatter"
                />
              </v-card-text>
            </v-card>
          </v-col>
        </v-row>

        <v-row>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                Crack growth rate vs fracture energy
                <info-tooltip>
                  Crack growth rate vs fracture energy.
                </info-tooltip>
              </v-card-title>
              <v-card-text>
                <simple-chart
                  :series="daDnVsGMBTSeries"
                  :aspect-ratio="2"
                  x-axis-name="G [J/m²]"
                  y-axis-name="da/dN [mm/cycle]"
                  :y-axis-type="'log'"
                  :y-axis-min="yAxisLogLimits_daDnVsGMBT.min"
                  :y-axis-max="yAxisLogLimits_daDnVsGMBT.max"
                  :y-axis-split-number="yAxisLogLimits_daDnVsGMBT.splitNumber"
                  :axis-label-formatter="axisTickFormatter"
                  :tooltip-formatter="formatScientific"
                />
              </v-card-text>
            </v-card>
          </v-col>
        </v-row>
      </v-col>
      <v-col cols="2">
        <v-card :loading="loading">
          <v-card-text>
            <ul>
              <li>
                <experiment-s-v
                  subject="Specimen number"
                  :values="specimenIds"
                  :colors="valueColors"
                  value-type="bigNumber"
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Cycle at failure"
                  :values="cycleAtFailure"
                  :colors="valueColors"
                  value-type="bigNumber"
                  tooltip="Number of cycles to failure. Plotted values can be up to the last DIC recorded value"
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Run out"
                  :values="runOuts"
                  :colors="valueColors"
                  tooltip="No fatigue failure."
                />
              </li>
            </ul>
          </v-card-text>
        </v-card>
      </v-col>
    </v-row>
    <v-row v-if="experimentType === 'QS' && !isFracture">
      <v-col cols="10">
        <v-card :loading="loading">
          <v-card-title>Strain vs Stress</v-card-title>
          <v-card-text>
            <v-row>
              <v-col>
                <v-select
                  v-model="strainOption"
                  :items="strainOptions"
                  :disabled="strainOptions.length < 2"
                  label="Strain"
                />
              </v-col>
              <v-col>
                <v-select
                  v-model="stressOption"
                  :items="stressOptions"
                  :disabled="stressOptions.length < 2"
                  label="Stress"
                />
              </v-col>
            </v-row>
            <simple-chart
              :series="strainStressSeriesQS"
              :aspect-ratio="2"
              x-axis-name="Strain [-]"
              y-axis-name="Stress [MPa]"
              :y-axis-max="yAxisMaxStrainStressQS"
              :axis-label-formatter="axisTickFormatter"
            />
          </v-card-text>
        </v-card>
      </v-col>
      <v-col cols="2">
        <v-card :loading="loading">
          <v-card-text>
            <ul>
              <li>
                <experiment-s-v
                  subject="Specimen number"
                  :values="specimenIds"
                  :colors="valueColors"
                  value-type="bigNumber"
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Max stress"
                  :values="formattedStressAtFailure"
                  :colors="valueColors"
                  :unit="units.stress"
                  tooltip="Maximum stress recorded in the test."
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Max strain"
                  :values="formattedStrainAtFailure"
                  :colors="valueColors"
                  unit="%"
                  tooltip="Maximum strain recorded in the test."
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Toughness"
                  :values="formattedToughnessValues"
                  :colors="valueColors"
                  :unit="'N/mm²'"
                  tooltip="Area under the stress-strain curve..."
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Young's modulus"
                  :values="formattedYoungModulusValues"
                  :colors="valueColors"
                  :unit="'GPa'"
                  tooltip="Linear slope of the stress-strain curve between 0.0015 and 0.0035 strain."
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Poisson ratio"
                  :values="formattedPoissonRatioValues"
                  :colors="valueColors"
                  :unit="'[-]'"
                  tooltip="Slope of the -eyy vs exx linear fit between 0.0015 and 0.0035 strain."
                />
              </li>
            </ul>
          </v-card-text>
        </v-card>
      </v-col>
    </v-row>
    <v-row v-else-if="experimentType === 'QS' && isFracture">
      <v-col cols="10">
        <v-row>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                <v-span>Load & Crack length vs Displacement</v-span>
                <info-tooltip>
                  The graph shows the evolution of the crack length (dotted
                  lines) and load (straight line) during the Displacement
                  Controlled test.
                </info-tooltip>
              </v-card-title>
              <v-card-text>
                <double-chart
                  :series="crackSeries"
                  :aspect-ratio="2"
                  x-axis-name="Displacement [mm]"
                  :x-axis-max="xAxisMaxDoubleChart"
                  :x-axis-min="xAxisMinDoubleChart"
                  :y1-axis-name="'Load [kN]'"
                  :y1-axis-max="y1AxisMaxDoubleChart"
                  :y1-axis-min="y1AxisMinDoubleChart"
                  :y2-axis-name="'Crack Length [mm]'"
                  :y2-axis-max="y2AxisMaxDoubleChart"
                  :y2-axis-min="y2AxisMinDoubleChart"
                  :axis-label-formatter="axisTickFormatter"
                />
              </v-card-text>
            </v-card>
          </v-col>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                <v-row align="center" class="w-100">
                  <v-col class="d-flex align-center" cols="auto">
                    Fracture Energy vs Crack Length
                    <info-tooltip>
                      The graph shows the evolution of the fracture energy
                      calculated with different methods as a function of the
                      <ul>
                        <li>
                          <strong>Modified Beam Theory, MBT</strong> = solid
                          line
                        </li>
                        <li>
                          <strong>Modified Compliance Calibration, MCC</strong>
                          = dashed line
                        </li>
                        <li>
                          <strong
                            >Experimental Compliance Calibration, ECM</strong
                          >
                          = dotted line
                        </li>
                      </ul>
                    </info-tooltip>
                  </v-col>
                  <v-spacer />
                  <v-col cols="auto">
                    <v-select
                      v-model="selectedFractureEnergyMethods"
                      :items="fractureEnergyMethodOptions"
                      label="Fracture Energy Types"
                      dense
                      hide-details
                      multiple
                      style="max-width: 220px"
                    >
                      <template #prepend-item>
                        <v-list-item
                          @click="selectedFractureEnergyMethods = []"
                        >
                          <v-list-item-title class="text-primary"
                            >Clear all</v-list-item-title
                          >
                        </v-list-item>
                        <v-divider />
                      </template>
                    </v-select>
                  </v-col>
                </v-row>
              </v-card-title>
              <v-card-text>
                <simple-chart
                  :series="fractureEnergySeriesCombined"
                  :aspect-ratio="2"
                  x-axis-name="Crack Length [mm]"
                  y-axis-name="Fracture Energy [J/m²]"
                  :y-axis-max="yAxisMaxFractureEnergyCombined"
                  :x-axis-max="xAxisMaxFractureEnergyCombined"
                  :axis-label-formatter="axisTickFormatter"
                />
              </v-card-text>
            </v-card>
          </v-col>
        </v-row>
      </v-col>
      <v-col cols="2">
        <v-card :loading="loading">
          <v-card-text>
            <ul>
              <li>
                <experiment-s-v
                  subject="Specimen number"
                  :values="specimenIds"
                  :colors="valueColors"
                  value-type="bigNumber"
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Initial crack length"
                  :values="formattedInitialCrackLengths"
                  :colors="valueColors"
                  :unit="'mm'"
                  tooltip="Initial crack length measured before testing."
                />
              </li>
            </ul>
          </v-card-text>
        </v-card>
      </v-col>
    </v-row>
  </v-container>
</template>

<script>
import DoubleChart from "@/components/charts/DoubleChart.vue";
import SimpleChart from "@/components/charts/SimpleChart.vue";
import ExperimentSpecifications from "@/components/ExperimentSpecifications.vue";
import ExperimentSV from "@/components/ExperimentSV.vue";
import InfoTooltip from "@/components/InfoTooltip.vue";
import {
  computeLogYAxisLimits,
  computeXAxisMax,
  computeXAxisMin,
  computeYAxisMax,
  computeYAxisMin,
  formatNumber0,
  formatNumber5,
  formatTick,
} from "@/utils/formatters";
import { colorPalette } from "@/utils/style";
import { zip } from "lodash";
import { mapState } from "vuex";

export default {
  name: "TestsDashboard",
  components: {
    DoubleChart,
    SimpleChart,
    ExperimentSpecifications,
    ExperimentSV,
    InfoTooltip,
  },
  props: {
    experimentId: { type: Number, required: true },
    testIds: { type: Array, required: true },
  },
  data() {
    return {
      loading: false,
      xAxisMode: "normal", // 'normal', 'log', 'normalized'
      yAxisStiffnessMode: "absolute", // 'absolute' or 'normalized'
      colors: colorPalette,
      fatigueData: [],
      cycleAtFailure: [],
      stressAtFailure: [],
      strainAtFailure: [],
      specimenIds: [],
      totalDissipatedEnergies: [],
      runOuts: [],
      selectedLoopIndices: [], // { value }
      crackSeries: [],
      strainData: {},
      strainOptions: [],
      strainOption: null,
      stressData: {},
      stressOptions: [],
      stressOption: null,
      toughnessValues: [],
      youngModulusValues: [],
      quasiStaticPoissonRatios: [],
      fatigueWarnings: [],
      fractureEnergyData_mbt: {},
      fractureEnergyData_mcc: {},
      fractureEnergyData_ecm: {},
      crackLengthData: {},
      specimenName: {},
      selectedFractureEnergyMethods: [], // selected methods (MBT, MCC, ECM)
      fractureEnergyMethodOptions: [
        { text: "MBT", value: "mbt" },
        { text: "MCC", value: "mcc" },
        { text: "ECM", value: "ecm" },
      ],
    };
  },
  computed: {
    // --- Vuex State ---
    ...mapState("experiments", {
      experiment: "oneExperiment",
      units: "units",
    }),

    // --- Experiment Type & Mode ---
    experimentType() {
      return this.experiment.experiment.experiment_type;
    },
    isFracture() {
      const exp = this.experiment?.experiment;
      const type = exp?.experiment_type?.toLowerCase();
      if (type === "qs") {
        return exp.qs_experiment_type?.toLowerCase() === "fracture";
      }
      if (type === "fa") {
        return exp.fa_experiment_type?.toLowerCase() === "fracture";
      }
      return false;
    },

    // --- Chart Data: Quasi-static Strain/Stress ---
    strainStressSeriesQS() {
      // Returns series for QS strain-stress chart
      return this.testIds
        .map((id) => ({
          type: "line",
          name: this.specimenName[id],
          data: zip(
            this.strainData[id]?.[this.strainOption] || [],
            this.stressData[id]?.[this.stressOption] || []
          ),
        }))
        .filter((s) => s.data.length);
    },

    // --- Chart Data: Fatigue Hysteresis Loops ---
    strainStressSeriesFA() {
      // Returns series for FA hysteresis loops chart
      return this.fatigueData.flatMap((test, testIndex) => {
        const loops = test.hysteresis_loops || [];
        const name = test.specimen_name;
        const color = this.colors[testIndex % this.colors.length];
        if (!loops.length) return [];
        const indices =
          this.selectedLoopIndices.length > 0
            ? this.selectedLoopIndices
            : Array.from({ length: loops.length }, (_, i) => i);
        return indices
          .map((i) => ({
            type: "line",
            name: null,
            data: zip(loops[i]?.strain || [], loops[i]?.stress || []),
            lineStyle: { color },
          }))
          .map((series, i) => ({
            ...series,
            name: i === 0 ? name : null,
          }));
      });
    },
    loopIndexOptions() {
      // Options for hysteresis loop selection
      return Array.from({ length: 10 }, (_, i) => ({
        text: `Cycle ${i + 1}`,
        value: i,
      }));
    },

    // --- Chart Data: Fatigue Evolution ---
    hysteresisAreaSeries() {
      // Hysteresis area evolution series
      return this.fatigueData.map((d) => {
        const normalizedX = this.transformXAxis(d.n_cycles, d.n_fail);
        const series = {
          type: "line",
          name: d.specimen_name,
          data: zip(normalizedX, d.hysteresis_area),
        };
        if (this.xAxisMode === "normalized") {
          series.rawX = d.n_cycles;
        }
        return series;
      });
    },
    creepSeries() {
      // Creep evolution series
      return this.fatigueData.map((d) => {
        const normalizedX = this.transformXAxis(d.n_cycles, d.n_fail);
        const series = {
          type: "line",
          name: d.specimen_name,
          data: zip(normalizedX, d.creep),
        };
        if (this.xAxisMode === "normalized") {
          series.rawX = d.n_cycles;
        }
        return series;
      });
    },
    stiffnessSeries() {
      // Stiffness evolution series
      return this.fatigueData.map((d) => {
        let yValues;
        if (this.yAxisStiffnessMode === "normalized") {
          yValues = this.normalizeYAxis(d.stiffness);
        } else {
          yValues = d.stiffness.map((v) =>
            typeof v === "number" ? v / 1000 : v
          );
        }
        const normalizedX = this.transformXAxis(d.n_cycles, d.n_fail);
        const series = {
          type: "line",
          name: d.specimen_name,
          data: zip(normalizedX, yValues),
        };
        if (this.xAxisMode === "normalized") {
          series.rawX = d.n_cycles;
        }
        return series;
      });
    },

    // --- Chart Data: Fatigue Fracture ---
    crackFaFractureSeries() {
      // Crack length & load vs cycles for FA fracture
      return this.fatigueData.flatMap((d, i) => {
        const id = d.specimen_id;
        const name = d.specimen_name || `Specimen ${id}`;
        const color = this.colors[i % this.colors.length];
        if (!d.crack_n_cycles?.length || !d.crack_load?.length) {
          console.warn("⚠️ Missing crack data for specimen", id);
          return [];
        }
        const transformedX = this.transformXAxis(d.crack_n_cycles, d.n_fail);
        const rawX = d.crack_n_cycles;
        return [
          {
            type: "line",
            name,
            data: zip(transformedX, d.crack_load),
            rawX: rawX,
            yAxisIndex: 0,
            lineStyle: { color },
            itemStyle: { color },
          },
          {
            type: "scatter",
            name: null,
            data: zip(transformedX, d.crack_length || []),
            rawX: rawX,
            yAxisIndex: 1,
            symbolSize: 6,
            itemStyle: { color },
          },
        ];
      });
    },
    fractureGMBTSeries() {
      // Fracture energy (MBT) vs cycles
      return this.fatigueData.map((d) => {
        const normalizedX = this.transformXAxis(d.crack_n_cycles, d.n_fail);
        const series = {
          type: "line",
          name: d.specimen_name,
          data: zip(normalizedX, d.G_MBT),
        };
        if (this.xAxisMode === "normalized") {
          series.rawX = d.crack_n_cycles;
        }
        return series;
      });
    },
    daDnVsGMBTSeries() {
      // Crack growth rate vs fracture energy
      return this.fatigueData.map((d) => ({
        type: "line",
        name: d.specimen_name,
        data: zip(d.G_MBT, d.da_dN),
      }));
    },

    // --- Chart Data: Fracture Energy (QS) ---
    fractureEnergySeriesCombined() {
      // Combined fracture energy series for QS fracture
      const lineStyles = {
        mbt: { type: "solid" },
        mcc: { type: "dashed" },
        ecm: { type: "dotted" },
      };
      const selectedMethods =
        this.selectedFractureEnergyMethods.length > 0
          ? this.selectedFractureEnergyMethods
          : ["mbt", "mcc", "ecm"];
      return this.testIds.flatMap((id, testIndex) => {
        const crackLength = this.crackLengthData[id];
        if (!crackLength) return [];
        const color = this.colors[testIndex % this.colors.length];
        return selectedMethods
          .map((method) => {
            const fractureEnergyData = this[`fractureEnergyData_${method}`][id];
            if (!fractureEnergyData) return null;
            return {
              type: "line",
              name: `${this.specimenName[id]}`,
              data: zip(crackLength, fractureEnergyData),
              lineStyle: { ...lineStyles[method], color },
              itemStyle: { color },
              tooltip: {
                formatter: function (params) {
                  return params.seriesName;
                },
              },
            };
          })
          .filter(Boolean);
      });
    },

    // --- Chart Axis & Limits ---
    computedXAxisLabel() {
      // X axis label for fatigue charts
      switch (this.xAxisMode) {
        case "log":
          return "log₁₀(Number of cycles) [-]";
        case "normalized":
          return "Normalized cycles (Number of cycles / Cycles at failure) [-]";
        default:
          return "Number of cycles [-]";
      }
    },
    xAxisChartType() {
      // X axis type for charts
      return this.xAxisMode === "log" ? "log" : "value";
    },
    computedYAxisStiffnessLabel() {
      // Y axis label for stiffness chart
      return this.yAxisStiffnessMode === "normalized"
        ? "Normalized stiffness [-]"
        : "Stiffness [GPa]";
    },

    // --- Chart Axis: Min/Max Calculations ---
    yAxisMaxStrainStressQS() {
      return computeYAxisMax(this.strainStressSeriesQS);
    },
    yAxisMaxStiffness() {
      return computeYAxisMax(this.stiffnessSeries);
    },
    yAxisMaxFractureEnergyCombined() {
      return computeYAxisMax(this.fractureEnergySeriesCombined);
    },
    xAxisMaxFractureEnergyCombined() {
      return computeXAxisMax(this.fractureEnergySeriesCombined);
    },
    yAxisMaxGMBT() {
      return computeYAxisMax(this.fractureGMBTSeries);
    },
    // Double chart axis min/max for both FA and QS fracture
    xAxisMaxDoubleChart() {
      const series =
        this.isFracture && this.experimentType === "QS"
          ? this.crackSeries
          : this.crackFaFractureSeries;
      return computeXAxisMax(series);
    },
    xAxisMinDoubleChart() {
      const series =
        this.isFracture && this.experimentType === "QS"
          ? this.crackSeries
          : this.crackFaFractureSeries;
      return computeXAxisMin(series);
    },
    y1AxisMaxDoubleChart() {
      const series =
        this.isFracture && this.experimentType === "QS"
          ? this.crackSeries
          : this.crackFaFractureSeries;
      const y1Series = series.filter((s) => s.yAxisIndex !== 1);
      return computeYAxisMax(y1Series);
    },
    y1AxisMinDoubleChart() {
      const series =
        this.isFracture && this.experimentType === "QS"
          ? this.crackSeries
          : this.crackFaFractureSeries;
      const y1Series = series.filter((s) => s.yAxisIndex !== 1);
      return computeYAxisMin(y1Series);
    },
    y2AxisMaxDoubleChart() {
      const series =
        this.isFracture && this.experimentType === "QS"
          ? this.crackSeries
          : this.crackFaFractureSeries;
      const y2Series = series.filter((s) => s.yAxisIndex === 1);
      return computeYAxisMax(y2Series);
    },
    y2AxisMinDoubleChart() {
      const series =
        this.isFracture && this.experimentType === "QS"
          ? this.crackSeries
          : this.crackFaFractureSeries;
      const y2Series = series.filter((s) => s.yAxisIndex === 1);
      return computeYAxisMin(y2Series);
    },
    yAxisLogLimits_daDnVsGMBT() {
      // Log axis limits for da/dN vs G chart
      const { min, max, splitNumber } = computeLogYAxisLimits(
        this.daDnVsGMBTSeries
      );
      return { min, max, splitNumber };
    },

    // --- Chart Formatting & Tooltip ---
    axisTickFormatter() {
      return formatTick;
    },
    TooltipFormatter_5() {
      return formatNumber5;
    },
    XAxis_cycles() {
      return formatNumber0;
    },
    xAxisTickFormatter() {
      // Custom tick formatter for x axis
      if (this.xAxisMode === "normal") {
        return this.XAxis_cycles();
      }
      return this.axisTickFormatter;
    },

    // --- Value Formatting for UI ---
    valueColors() {
      // Color for each specimen/test
      return this.testIds.map((_, i) => this.colors[i % this.colors.length]);
    },
    formattedStressAtFailure() {
      return this.stressAtFailure.map((s) =>
        s != null ? Number(s).toFixed(2) : "-"
      );
    },
    formattedStrainAtFailure() {
      return this.strainAtFailure.map((e) =>
        e != null ? Number(e).toFixed(4) : "-"
      );
    },
    formattedToughnessValues() {
      return this.toughnessValues.map((t) =>
        t != null ? Number(t).toFixed(2) : "-"
      );
    },
    formattedInitialCrackLengths() {
      return this.initialCrackLengths.map((v) =>
        v != null ? Number(v).toFixed(2) : "-"
      );
    },
    formattedYoungModulusValues() {
      return this.youngModulusValues.map((v) =>
        v != null ? Number(v).toFixed(2) : "-"
      );
    },
    formattedPoissonRatioValues() {
      return this.quasiStaticPoissonRatios?.map((v) =>
        v != null && isFinite(v) ? Number(v).toFixed(3) : "-"
      );
    },
    formattedTotalDissipatedEnergies() {
      return this.totalDissipatedEnergies.map((e) =>
        e != null ? Number(e).toFixed(2) : "-"
      );
    },

    // --- Warnings ---
    hasWarnings() {
      // True if any fatigue test has warnings
      return this.testIds.some((_, i) => this.fatigueWarnings?.[i]);
    },
  },
  watch: {
    experimentType: {
      immediate: true,
      handler(val) {
        if (!val) return;
        this.loading = true;
        if (val === "FA") {
          this.loadFatigue();
        } else if (val === "QS") {
          this.loadQuasiStatic();
        }
      },
    },
  },
  created() {
    this.$store.dispatch("experiments/fetchOneExperimentWithTests", {
      experimentId: this.experimentId,
      pagination: { page: 1, size: 20 },
    });
  },
  methods: {
    async loadFatigue() {
      const dataList = await Promise.all(
        this.testIds.map((tid) =>
          this.$experimentsApi.getFatigueTest(this.experimentId, tid)
        )
      );
      this.specimenName = {};
      this.cycleAtFailure = [];
      this.stressAtFailure = [];
      this.strainAtFailure = [];
      this.specimenIds = [];
      this.totalDissipatedEnergies = [];
      this.runOuts = [];
      this.fatigueData = dataList;
      dataList.forEach((d, i) => {
        const tid = this.testIds[i];
        this.specimenName[tid] = d.specimen_name;
        this.cycleAtFailure.push(d.n_fail);
        this.stressAtFailure.push(d.stress_at_failure);
        this.strainAtFailure.push(d.strain_at_failure);
        this.specimenIds.push(d.specimen_id);
        this.totalDissipatedEnergies.push(d.total_dissipated_energy);
        this.runOuts.push(d.run_out);
        this.fatigueWarnings.push(d.warning_messages || false);
        d.crack_length = d.crack_length || [];
        d.crack_n_cycles = d.crack_n_cycles || [];
        d.crack_load = d.crack_load || [];
        d.G_MBT = d.G_MBT || [];
        d.da_dN = d.da_dN || [];
      });

      this.loading = false;
    },

    async loadQuasiStatic() {
      const dataList = await Promise.all(
        this.testIds.map((tid) =>
          this.$experimentsApi.getQuasiStaticTest(this.experimentId, tid)
        )
      );

      this.specimenName = {};
      this.crackSeries = [];
      this.strainData = {};
      this.strainOptions = new Set();
      this.stressData = {};
      this.stressOptions = new Set();
      this.specimenIds = [];
      this.stressAtFailure = [];
      this.strainAtFailure = [];
      this.toughnessValues = []; // new
      this.youngModulusValues = []; // new
      this.quasiStaticPoissonRatios = []; // new
      this.initialCrackLengths = []; // new
      this.crackLengthData = {};

      dataList.forEach((d, i) => {
        const tid = this.testIds[i];
        this.specimenName[tid] = d.specimen_name;
        this.fractureEnergyData_mbt[tid] = d.crack_fractureenergy_mbt;
        this.fractureEnergyData_mcc[tid] = d.crack_fractureenergy_mcc;
        this.fractureEnergyData_ecm[tid] = d.crack_fractureenergy_ecm;
        this.crackLengthData[tid] = d.crack_length;
        if (d.crack_displacement.length) {
          this.crackSeries.push({
            type: "line",
            name: d.specimen_name,
            data: zip(d.crack_displacement, d.crack_load),
          });
          this.crackSeries.push({
            type: "scatter",
            name: d.specimen_name,
            yAxisIndex: 1,
            symbolSize: 6,
            data: zip(d.crack_displacement, d.crack_length),
          });
        }

        Object.keys(d.strain).forEach((k) => this.strainOptions.add(k));
        Object.keys(d.stress).forEach((k) => this.stressOptions.add(k));
        this.strainData[tid] = d.strain;
        this.stressData[tid] = d.stress;
        if (typeof d.specimen_id !== "undefined") {
          this.specimenIds.push(String(d.specimen_id));
        } else {
          console.warn("⚠️ specimen_id missing for test", tid);
          this.specimenIds.push("–"); // or "N/A" for visual clarity
        }
        const stressValues = Object.values(d.stress)
          .flat()
          .filter(Number.isFinite);
        const strainValues = Object.values(d.strain)
          .flat()
          .filter(Number.isFinite);
        this.stressAtFailure.push(Math.max(...stressValues));
        this.strainAtFailure.push(Math.max(...strainValues));
        this.toughnessValues.push(
          isFinite(d.toughness) ? Number(d.toughness) : null
        );
        this.initialCrackLengths.push(
          isFinite(d.initial_crack_length)
            ? Number(d.initial_crack_length)
            : null
        );
        this.youngModulusValues.push(
          isFinite(d.young_modulus) ? Number(d.young_modulus) : null
        );
        this.quasiStaticPoissonRatios.push(d.poisson_ratio);
      });

      this.strainOptions = Array.from(this.strainOptions);
      this.strainOption = this.strainOptions[0] || null;
      this.stressOptions = Array.from(this.stressOptions);
      this.stressOption = this.stressOptions[0] || null;

      this.loading = false;
    },

    goBack() {
      this.$router.go(-1);
    },
    transformXAxis(xValues, nFail) {
      if (this.xAxisMode === "normalized") {
        return xValues.map((x) => x / (nFail || 1)); // avoid division by zero
      }
      // 'normal' and 'log': return raw data (the log is handled by SimpleChart.vue)
      return xValues;
    },
    normalizeYAxis(values) {
      if (!Array.isArray(values) || values.length === 0) return values;
      const base = values.find((v) => typeof v === "number" && isFinite(v));
      if (!base || base === 0) return values;
      return values.map((v) => (isFinite(v) ? v / base : v));
    },
    formatScientific(value) {
      if (typeof value !== "number" || !isFinite(value)) return value;
      return value.toExponential(2); // ad esempio 2 cifre decimali
    },
  },
};
</script>
