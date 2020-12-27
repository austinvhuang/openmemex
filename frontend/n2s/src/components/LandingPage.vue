<template>
  <div id="landing-page">
    {{ entry.date }} : {{ showContent }}
    <p />
    <strong>tags:</strong>

    <div>
      <li v-for="tag in tags" v-bind:key="tag">{{ tag }}</li>
    </div>
    <p />
    <button @click="queryTags">
      Query tags
    </button>
    <p />
    <strong>query count:</strong> {{ queryCount }}
    <p />

    <textarea v-model="searchQuery" class="searchbox" rows="1" maxLength="60" />
    <br />
    <button v-show="searchQuery" type="submit">Search</button>
    <p v-if="searchQuery != ''">{{ searchQuery }}</p>
    <p v-else>No query entered.</p>
    <div class="query-badge" v-if="!hasSearchContent">
      No search query entered.
    </div>
  </div>
</template>

<script>
export default {
  name: "LandingPage",
  data() {
    return {
      entry: {
        entryID: 1,
        date: "2020-12-01",
        time: "",
        content: "hello world"
      },
      tags: ["hi", "there"],
      queryCount: 0,
      searchQuery: ""
    };
  },
  watch: {
    queryCount(oldVal, newVal) {
      console.log(`${oldVal} to ${newVal}`);
    }
  },
  computed: {
    showContent() {
      return `Entry ${this.entry.entryID} ${this.entry.date} ${this.entry.content}`;
    },
    hasSearchContent() {
      console.log(this.searchQuery);
      return this.searchQuery != "";
    }
  },
  methods: {
    queryTags() {
      this.tags = ["bye", "there"];
      this.queryCount++;
    }
  },
  mounted: function() {
    console.log("mounted");
    this.queryTags();
    fetch("http://localhost:8080/all/tags/")
      .then(response => response.json())
      .then(data => {
        console.log("hello world");
        console.log(data);
        var r = data.map(x => x[0]);
        console.log(r);
        this.tags = r;
        return r;
      });
  }
};
</script>

<style>
.query-badge {
  background: red;
  color: white;
  border-radius: 5px;
  margin-right: auto;
  padding: 0 10px;
  font-weight: bold;
  flex-direction: column;
}
</style>
