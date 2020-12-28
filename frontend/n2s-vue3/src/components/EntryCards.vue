<template>
  <div class="container">
    <button v-tooltip.auto="msg">test</button>
    <svg height="100" width="100%" v-tooltip.top-center="msg">
      <line
        x1="0"
        y1="50%"
        x2="100%"
        y2="50%"
        style="stroke: rgb(40, 40, 40); stroke-width: 1"
      />

      <circle
        v-for="item in parsedDate"
        :key="item.entryID"
        :cx="item.pDate + '%'"
        cy="50%"
        r="4"
        stroke="#666666"
        stroke-width="1"
        fill="#CCCCCC"
        opacity="0.1"
      />
    </svg>

    <button v-on:click="cache_run = 1">Run cache</button>

    {{ cache_run }}
    <p></p>

    <div class="card" v-for="item in entries" :key="item.entryID">
      <div class="card-header">
        <h3><span v-text="item.date" /></h3>
      </div>
      <span v-text="item.content" />
    </div>
  </div>
</template>

<script>
export default {
  data() {
    return {
      entries: [],
      parsedDate: [],
      cache_run: 0,
      msg: "test",
    };
  },
  computed: {},
  created: function () {
    // note - uses proxy
    fetch("http://localhost:8080/all/entries")
      .then((response) => response.json())
      .then((data) => {
        console.log("retrieved data");
        this.entries = data;
        return data;
      })
      .then((data) => {
        let parsed = data.map((e) => Date.parse(e.date));
        let maxVal = Math.max(...parsed);
        let minVal = Math.min(...parsed);
        this.parsedDate = this.entries.map((e) => ({
          entryID: e.entryID,
          pDate: ((Date.parse(e.date) - minVal) / (maxVal - minVal)) * 100,
        }));
        console.log(this.parsedDate);
      });
  },
  mounted: function () {},
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
      console.log(this.parsedDate);
    },
    entries(before, after) {
      console.log("before");
      console.log(before);
      console.log("after");
      console.log(after);
      console.log("updated parsed date");
    },
  },
};
</script>

<style>
.card {
  /* box-shadow: 10px, 10px grey; */
  box-shadow: 0px 5px 10px 0px rgba(0, 0, 0, 0.2),
    0px 4px 4px 0px rgba(0, 0, 0, 0.15), 0px 2px 2px 0px rgba(0, 0, 0, 0.2);
  background-color: #eeeeee;
  border-radius: 10px;
  width: 80%;
  float: left;
  margin-bottom: 5px;
  margin-top: 5px;
  padding: 10px;
  text-align: left;
}

.card-header {
  background-color: #ffaaaa;
}

.container {
  width: 80%;
  margin: 0 auto;
}
</style>
