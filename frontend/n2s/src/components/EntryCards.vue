<template>
  <h1>Entries</h1>

  <button v-on:click="cache_run = 1">Run cache</button>

  {{ cache_run }}
  <p></p>

  <div class="card" v-for="item in entries" :key="item.entryID">
    <span v-text="item.content" />
  </div>
</template>
<script>
export default {
  data() {
    return {
      entries: {},
      cache_run: 0
    };
  },
  created: function() {},
  mounted: function() {
    // note - use proxy
    fetch("http://localhost:8080/all/entries")
      .then(response => response.json())
      .then(data => {
        console.log("retrieved data");
        this.entries = data;
      });
  },
  methods: {},
  watch: {
    cache_run(before, after) {
      console.log("before");
      console.log(before);
      console.log("after");
      console.log(after);
      this.cache_run = 0;
      console.log("Cache Reset");
      console.log(this.cache_run);
    }
  }
};
</script>
<style>
.card {
  box-shadow: 10px, 10px grey;
  background-color: #cccccc;
  border-radius: 5px;
  width: 100%;
  float: left;
  margin-bottom: 5px;
  margin-top: 5px;
  padding: 10px;
}
</style>
