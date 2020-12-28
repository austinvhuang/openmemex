<template>
  <h1>note2self.ai</h1>

  <div id="app">
    <EntryCards />
  </div>
</template>

<script>
//import LandingPage from "./components/LandingPage"
// import Vue from "vue";
import EntryCards from "./components/EntryCards";
// import Tooltip from "v-tooltip";

// Vue.use(Tooltip);

export default {
  name: "App",
  components: { EntryCards },
};
</script>

<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}
</style>
