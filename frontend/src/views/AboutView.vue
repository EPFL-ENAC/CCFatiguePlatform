<template>
  <v-container>
    <h1>About</h1>
    <v-card flat>
      <v-card-title>Disclaimer</v-card-title>
      <p class="text-body-1 text-justify">
        CCFatiguePlatform is an initiative from GR-MeC at EPFL that aims to
        develop a open-source web application to harmonize, facilitate storage
        and analysis of composite materials testing datasets.
      </p>
      <p class="text-body-1 text-justify">
        The web app is under active development and at prototype stage. Please
        report any bugs as Github Issues in the code repository.
      </p>
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
                <!--
                -
                {{ contributor.title }}
                -->
              </li>
            </ul>
          </v-col>
        </v-row>
      </v-card-text>
    </v-card>
    <div class="logo-row py-5">
      <img
        src="/img/epfl-logo.svg"
        alt="Logo EPFL, École polytechnique fédérale de Lausanne"
      />
      <img
        src="/img/GR_MecLogo.png"
        alt="Logo GR-Mec"
        class="ml-6 grmec-logo"
      />
    </div>
  </v-container>
</template>

<script>
export default {
  name: "AboutView",
  data() {
    return {
      units: [
        { name: "GR-MeC", url: "https://gr-mec.epfl.ch" },
        { name: "ENAC-IT4R", url: "https://enacit4r.epfl.ch/" },
      ],
      contributors: [
        {
          name: "Tassos Vassilopoulos",
          url: "https://people.epfl.ch/anastasios.vassilopoulos",
          unit: "GR-MeC",
        },
        {
          name: "Filippo Mannino",
          url: "https://people.epfl.ch/filippo.mannino",
          unit: "GR-MeC",
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
        {
          name: "David Tang",
          url: "https://people.epfl.ch/david.tang",
          unit: "ENAC-IT4R",
          title: "Ing data + dev",
        },
        {
          name: "Pierre Guilbert",
          url: "https://people.epfl.ch/pierre.guilbert",
          unit: "ENAC-IT4R",
          title: "Ing data + dev",
        },
        {
          name: "Yannick Marcon",
          url: "https://people.epfl.ch/yannick.marcon",
          unit: "ENAC-IT4R",
          title: "Ing data + dev",
        },
        {
          name: "Nicolas Dubois",
          url: "https://people.epfl.ch/nicolas.dubois",
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

<style>
ul {
  list-style-type: none;
}

.logo-row {
  display: flex;
  align-items: center;
  gap: 2rem;
}

.logo-row img {
  max-height: 50px;
  object-fit: contain;
}

.grmec-logo {
  max-height: 70px !important;
}
</style>
