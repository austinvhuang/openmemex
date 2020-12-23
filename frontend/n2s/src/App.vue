<template>
  <h1>memexAI</h1>

  <div id="app">
  <LandingPage/>
  </div>



</template>

<script>
import LandingPage from "./components/LandingPage"

export default {
  name: 'App',
  components: { LandingPage }
}
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
