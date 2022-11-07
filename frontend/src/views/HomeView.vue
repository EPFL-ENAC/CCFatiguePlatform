<template>
  <v-container>
    <h1>Home</h1>

    <v-card flat>
      <v-card-title> Introduction </v-card-title>
      <p>
        CCFatigue is an initiative from the Composite Construction Laboratory
        (CCLab) that aims at facilitating access to and analysis of composite
        materials testings. It is comprised of a database environment allowing
        for organized storage and retrieval of new and already existing data.
        Additionally to the database, an analysis tool is made available to
        allow for computation, visualization and comparison of experimental
        data.
      </p>
    </v-card>

    <v-card flat>
      <v-card-title>
        Web database for composite materials testings:
      </v-card-title>
      <p>
        Additionally to the analysis tool, the CCFatigue Web App includes a
        database environment that allows storage and management of raw data
        files. The database environment allows easy navigation and visualization
        of existing data and provides an intuitive environment for storage of
        composite materials testings. The database offers access to:
      </p>

      <ul>
        <li>Complete experimental setup of experiments</li>
        <li>Raw experimental data files</li>
        <li>Test configuration</li>
        <li>Graphs detailing the fatigue life of experiment</li>
      </ul>
    </v-card>

    <v-card flat>
      <v-card-title>
        Web based analysis tool for composite materials:
      </v-card-title>
      <p>
        The CCFatigue Web App allows for quick and precise analysis of fatigue,
        fracture and quasi-static composite material testings. The analysis tool
        includes complete fatigue life analysis for single testings as well as a
        comparative analysis app to compare results over one or several
        experiments or testings. The analysis tool has two distinct
        applications, the first one being the analysis of the complete fatigue
        life of single samples with the possibility of viewing results such as
        the evolution of Stiffness, Hysteresis Area, Creep as well as the value
        for total dissipated energy (TDE).
      </p>
      <p>
        Additionally, our analysis tool allows for visualization of results over
        a complete experiment with modules allowing computations such as Cycle
        Counting, drawing S-N curves, computing CLD, Fatigue Failure criterion
        as well as damage summation.
      </p>
    </v-card>
  </v-container>
</template>

<script>
export default {
  name: "HomeView",
};
</script>

<style scoped></style>
