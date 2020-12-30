<template>
  <div id="app"><EntryCards /></div>
</template>

<script>
import EntryCards from "./components/EntryCards";
// import EntryMap from "./components/EntryMap";

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
