<template>
  <div id="landing-page">
    {{ entry.date }} : {{ showContent }} 
    <p/>
    <strong>tags:</strong> {{ tags }}
    <p/>
    <button @click="queryTags">
    Query tags
    </button>
    <p/>
    <strong>query count:</strong> {{ queryCount }}
    <p/>

    <textarea v-model="searchQuery" class="searchbox" rows="1" maxLength="60"/>
    <br>
    <button v-show="searchQuery" type="submit">Search</button>
    <p v-if="searchQuery != ''">{{ searchQuery }}</p>
    <p v-else>No query entered.</p>

  </div>
</template>

<script>
export default {
  name: 'LandingPage',
  data() {
    return {
      entry: {
        entryID: 1,
        date: '2020-12-01',
        time: '',
        content: 'hello world'
      },
      tags: ["hi", "there"],
      queryCount: 0,
      searchQuery: "Search goes here"
    }
  },
  watch: {
    queryCount(oldVal, newVal) {
      console.log(`${oldVal} to ${newVal}`)
    }
  },
  computed: {
    showContent() {
      return `Entry ${this.entry.entryID} ${this.entry.date} ${this.entry.content}`
    }
  },
  methods: {
    queryTags() {
      this.tags = ['bye', 'there']
      this.queryCount++
    }
  },
  mounted: function() {
      console.log('mounted')
      this.queryTags()
    }
}
</script>