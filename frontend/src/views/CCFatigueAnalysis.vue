<template>
  <v-container fluid>
    <p>- not implemented yet -</p>
    <v-row>
      <v-col cols="6">
        <cycle-counting></cycle-counting>
      </v-col>
      <v-col cols="6">
        <sn-curve></sn-curve>
      </v-col>
      <v-col cols="6">
        <v-card>
          <v-card-title>
            CLD
            <v-select
              class="mx-5"
              :items="cldMethods"
              label="select CLD method(s)"
            >
            </v-select>
            <v-spacer />
            <info-button>
              <template v-slot:title> CLD </template>
              The Constant Life Diagram (CLD) allows us to predict the outcome
              of tests for different values of stress ratio (R). The results are
              represented in a Mean Stress - Stress Amplitude plane and define
              what is considered to be the safe use region.
            </info-button>
          </v-card-title>
          <v-container fluid>
            <v-row class="mx-2">
              <v-img src="/img/results_dashboard2.png"> </v-img>
              <v-spacer />
              <info-button>
                <template v-slot:title> CLD </template>
                The data required as input is a csv file with 3 distinct
                columns:
                <ul>
                  <li>Stress ratio (R) [-]</li>
                  <li>Number of cycles to failure</li>
                  <li>Stress parameter (Stress at failure) [MPa]</li>
                </ul>
                Additionally to these 3 columns, there are 11 values required
                for analysis:
                <ul>
                  <li>R-ratio</li>
                  <li>PN(f) !Reliability level</li>
                  <li>So !(c^(-1/b))</li>
                  <li>1/k ! (1/b)? -> Pow in fortran code</li>
                  <li>af pooled data</li>
                  <li>Scale pooled data</li>
                  <li>LRSQ</li>
                  <li>RMSE !Root mean square error</li>
                  <li>SSE !Sum of squares due to errors</li>
                  <li>SST !Sum of squares about the mean</li>
                  <li>RSQ !R-square</li>
                </ul>
              </info-button>
            </v-row>
            <v-row justify="end">
              <v-btn class="ma-5">Run</v-btn>
            </v-row>
          </v-container>
        </v-card>
      </v-col>

      <v-col cols="6">
        <v-card>
          <v-card-title>
            Fatigue Failure
            <v-select
              class="mx-5"
              :items="fatigueFailureMethods"
              label="select fatigue failure method(s)"
            >
            </v-select>
            <v-spacer />
            <info-button>
              <template v-slot:title> Fatigue Failure </template>
              The fatigue failure module allows us to predict the S-N curve that
              will arise from off axis loadings. It is designed for experiments
              where samples are subjected to multiaxial loadings.
            </info-button>
          </v-card-title>
          <v-container fluid>
            <v-row class="mx-2">
              <v-img src="/img/results_dashboard3.png"> </v-img>
              <v-spacer />
              <info-button>
                <template v-slot:title> Fatigue Failure </template>
                <p>
                  The data type selection box allows us to enter static, fatigue
                  and reference data that are necessary to the analysis, the
                  values that should be placed as input are dependent on the
                  method used for analysis. Static data have to do with the
                  static parameters of an experiment, they give information
                  relative to the strength of the material. Fatigue data are
                  related to the fatigue analysis, they give information about
                  the fatigue life of the experiment. Reference data are related
                  to the data we wish to extract from the analysis, they give
                  information about the orientation in which we seek more
                  information.
                </p>
                <p>
                  The inputs to provide highly depend on the selected method but
                  there are two main formats used as input data:
                </p>
                <p>
                  MKawai & Shorkieh-Taheri: (Same as input of S-N curve module)
                  Aggregated test results with 6 distinct columns:
                </p>
                <ul>
                  <li>Stress ratio (R) [-]</li>
                  <li>Reliability level (input parameter)</li>
                  <li>Stress level no. (estimated)</li>
                  <li>Stress parameter [MPa]</li>
                  <li>Number of cycles</li>
                  <li>Residual strength [MPa]</li>
                </ul>
                <br />
                <p>
                  Fawaz-Ellyin, FTPF, Hashim-Rothem, Sims-Brogdon: (Same as
                  output of S-N curve module)
                </p>
                <ul>
                  <li>Stress ratio (R) [-]</li>
                  <li>Number of cycles to failure</li>
                  <li>Stress parameter (Stress at failure) [MPa]</li>
                </ul>
                <br />
                <p>
                  Additionally to these 3 columns, there are 11 values required
                  for analysis:
                </p>
                <ul>
                  <li>R R-ratio</li>
                  <li>PN(f) !Reliability level</li>
                  <li>So !(c^(-1/b))</li>
                  <li>1/k ! (1/b)? -> Pow in fortran code</li>
                  <li>af pooled data</li>
                  <li>Scale pooled data</li>
                  <li>LRSQ</li>
                  <li>RMSE !Root mean square error</li>
                  <li>SSE !Sum of squares due to errors</li>
                  <li>SST !Sum of squares about the mean</li>
                  <li>RSQ !R-square</li>
                </ul>
              </info-button>
            </v-row>
            <v-row justify="end">
              <v-btn class="ma-5">Run</v-btn>
            </v-row>
          </v-container>
        </v-card>
      </v-col>

      <v-col cols="6">
        <v-card>
          <v-card-title>
            Damage Summation (uniaxial)
            <v-select
              class="mx-5"
              :items="dammageSummationMethods"
              label="select dammage summation method"
            >
            </v-select>
            <v-spacer />
            <info-button>
              <template v-slot:title> Damage Summation (uniaxial) </template>
              The damage summation module is based on the Palgrem-Miner linear
              damage rule; it allows us to define failure in a numerical
              fashion. This method is particularly interesting when dealing with
              varying amplitude loading, as it can be tedious to keep track of
              the damage inflicted on the sample for each loading/unloading
              cycle.
            </info-button>
          </v-card-title>
          <v-container fluid>
            <v-row class="mx-2">
              <v-img src="/img/results_dashboard4.png"> </v-img>
              <v-spacer />
              <info-button>
                <template v-slot:title> Damage Summation (uniaxial) </template>
                The data used as input for this method is slightly different
                than that of the previous modules. As damage summation deals
                with the fatigue life as a whole, we use the results from the
                previous analysis. We first browse for the load spectrum which
                is presented as a txt file with one numerical column
                representing the peaks and valleys of the loading. We then
                select the method we will use for counting the cycles, and
                finally choose the appropriate methods for CLD and for S-N
                curves/fatigue failure depending on whether we are in presence
                of a uniaxial or multiaxial loading.
              </info-button>
            </v-row>
            <v-row justify="end">
              <v-btn class="ma-5">Run</v-btn>
            </v-row>
          </v-container>
        </v-card>
      </v-col>
    </v-row>
  </v-container>
</template>

<script>
import InfoButton from "@/components/InfoButton";
import SnCurve from "@/components/analysis/SnCurve.vue";
import CycleCounting from "@/components/analysis/CycleCounting.vue";

export default {
  name: "CCFatigueAnalysis",
  components: {
    CycleCounting,
    InfoButton,
    SnCurve,
  },
  data() {
    return {
      cldMethods: [
        "Boerstra",
        "Kawai",
        "Linear",
        "Piecewise-Linear",
        "Piecewise-Nonlinear",
      ],
      fatigueFailureMethods: ["Fawaz-Ellyin", "Kawai", "ST"],
      dammageSummationMethods: ["Linear", "Piecewise-Linear"],
    };
  },
};
</script>
