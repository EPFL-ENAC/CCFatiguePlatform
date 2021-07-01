<template>
  <div class="this-page">
    <h1>Fatigue Test Dashboard - Individual test</h1>
    <Panel header="Test specifications">
      <div class="card">
        <div class="p-d-flex space-evenly">
          <div class="spec_column">
            <h4>General informations</h4>
            <ul>
              <li>
                Specimen: TODO
              </li>
              <li>
                Reference: TODO
              </li>
              <li>
                Researcher: TODO
              </li>
              <li>
                DOI: TODO
              </li>
            </ul>
          </div>
          <Divider layout="vertical" />
          <div class="spec_column">
            <h4>Test conditions</h4>
            <ul>
              <li>
                Temperature: TODO
              </li>
              <li>
                Relative humidity: TODO
              </li>
              <li>
                Grip pressure: TODO
              </li>
              <li>
                Measuring equipment: TODO
              </li>
            </ul>
          </div>
          <Divider layout="vertical" />
          <div class="spec_column">
            <h4>Laminate and assemblies</h4>
            <ul>
              <li>
                Curing time: TODO
              </li>
              <li>
                Curing temperature: TODO
              </li>
              <li>
                Curing Pressure: TODO
              </li>
              <li>
                Stacking sequence: TODO
              </li>
              <li>
                Fiber content: TODO
              </li>
            </ul>
          </div>
          <Divider layout="vertical" />
          <div class="spec_column">
            <h4>Materials</h4>
            <ul>
              <li>
                Resin: TODO
              </li>
              <li>
                Hardener: TODO
              </li>
              <li>
                Mixing ration: TODO
              </li>
              <li>
                Fiber Material: TODO
              </li>
              <li>
                Fiber Geometry: TODO
              </li>
              <li>
                Area Density: TODO
              </li>
            </ul>
          </div>
        </div>
      </div>
    </Panel>
  </div>
</template>

<script>
import Panel from 'primevue/panel'
import Divider from 'primevue/divider'

export default {
  name: 'DataBase',
  components: {
    Panel,
    Divider,
  }
}
</script>

<style scoped>
div.this-page {
  text-align: left;
}
div.space-evenly {
  justify-content: space-evenly;
}
ul {
  padding-left: 20px;
}
</style>
