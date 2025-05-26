<template>
  <v-container>
    <h1>About</h1>
    <v-card flat>
      <v-card-title>Disclaimer</v-card-title>
      <p class="text-body-1 text-justify">
        <em>CCFatigue</em>
        Platform is an initiative from GR-MeC at EPFL that aims to develop an
        open-source web application to harmonize, facilitate storage and
        analysis of composite materials testing datasets.
      </p>
      <p class="text-body-1 text-justify">
        The web app is under active development and at prototype stage. Please
        report any bugs as Github Issues in the code repository.
      </p>
    </v-card>
    <v-card flat>
      <v-card-title>Contributors</v-card-title>
      <p class="text-body-1 text-justify">
        <strong>GR-MeC</strong> -
        <a href="https://gr-mec.epfl.ch" target="_blank">
          https://gr-mec.epfl.ch
        </a>
        with the support of <strong>ENAC-IT4R</strong>
      </p>
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
