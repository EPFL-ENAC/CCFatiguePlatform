<template>
  <v-container>
    <v-card flat>
      <v-card-title>About</v-card-title>
      <v-card-text>
        <p class="text-justify">
          CCFatiguePlatform is an initiative from CCLab at EPFL that aims to
          develop a open-source web application to harmonize, facilitate storage
          and analysis of composite materials testing datasets.
        </p>
        <p class="text-justify">
          The web app is under active development and at prototype stage. Please
          report any bugs as Github Issues in the code repository.
        </p>
      </v-card-text>
    </v-card>

    <v-card flat>
      <v-card-title>Contributors</v-card-title>
      <v-card-text>
        <v-row>
          <v-col v-for="unit in units" :key="unit.name">
            <h3>
              {{ unit.name }} -
              <a :href="unit.url" target="_blank"> {{ unit.url }} </a>
            </h3>
            <ul>
              <li
                v-for="contributor in contributorsByUnit(unit.name)"
                :key="contributor.name"
              >
                <v-icon>mdi-account</v-icon>
                <a :href="contributor.url" target="_blank">
                  {{ contributor.name }}
                </a>
                -
                {{ contributor.title }}
              </li>
            </ul>
          </v-col>
        </v-row>
      </v-card-text>
    </v-card>

    <p class="py-5">
      <img
        src="/img/epfl-logo.svg"
        alt="Logo EPFL, École polytechnique fédérale de Lausanne"
      />
    </p>
  </v-container>
</template>

<script>
export default {
  name: "AboutView",
  data() {
    return {
      units: [
        { name: "CCLAB", url: "https://cclab.epfl.ch" },
        { name: "ENAC-IT4R", url: "https://enacit4r.epfl.ch/" },
      ],
      contributors: [
        {
          name: "Tassos Vassilopoulos",
          url: "https://people.epfl.ch/anastasios.vassilopoulos",
          unit: "CCLAB",
          title: "professor",
        },
        {
          name: "Congzhe Wang",
          url: "https://people.epfl.ch/congzhe.wang",
          unit: "CCLAB",
          title: "PhD",
        },
        {
          name: "Dharun Vadugappatty Srinivasan",
          url: "https://people.epfl.ch/dharun.srinivasan",
          unit: "CCLAB",
          title: "PhD",
        },
        {
          name: "Seyed Shayan Khalooei Tafti",
          url: "https://people.epfl.ch/shayan.khalooei",
          unit: "CCLAB",
          title: "PhD",
        },
        {
          name: "Scott Salmon",
          url: "https://people.epfl.ch/scott.matthewssalmon",
          unit: "CCLAB",
          title: "master student",
        },
        {
          name: "Charlie Weil",
          url: "https://people.epfl.ch/charlotte.weil",
          unit: "ENAC-IT4R",
          title: "Data scientist",
        },
        {
          name: "Samuel Bancal",
          url: "https://people.epfl.ch/samuel.bancal",
          unit: "ENAC-IT4R",
          title: "Ing dev",
        },
      ],
    };
  },
  methods: {
    contributorsByUnit(unit) {
      return this.contributors.filter(
        (contributor) => contributor.unit === unit
      );
    },
  },
};
</script>

<style scoped>
ul {
  list-style-type: none;
}
</style>
