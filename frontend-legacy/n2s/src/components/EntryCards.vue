<template>
  <div class="container">
    <div
      class="searchdiv"
      :style="{
        'box-shadow': searchStatus
          ? '0px 5px 10px 0px rgba(173, 154, 154, 0.6)'
          : '0px 5px 10px 0px rgba(173, 154, 154, 0.0)'
      }"
      @mouseover="searchOver($event, true)"
      @mouseleave="searchOver($event, false)"
    >
      Search:
      <input
        type="text"
        id="search"
        name="search"
        width="90%"
        class="searchinput"
      />
    </div>
    <svg
      height="100"
      width="100%"
      class="timeline fade-in"
      v-on:mousemove="drag($event)"
    >
      <line
        x1="0"
        y1="50%"
        x2="100%"
        y2="50%"
        style="stroke: rgb(40, 40, 40); stroke-width: 1"
      />

      <circle
        v-on:click="timelineClick($event, item.url)"
        v-for="item in parsedDate"
        v-tooltip.top-center="item.pageTitle"
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

    <!-- this width doesn't work, width is determined by the card CSS attribute -->
    <div width="66%">
      <div class="card fade-in" v-for="item in entries" :key="item.cvForeignID">
        <div class="card-header">
          <h3><span v-text="item.cvDate" /></h3>
        </div>
        <span v-text="item.cvContent" />
      </div>
    </div>
    <div width="34%">
      <object
        data="https://www.cnn.com"
        height="800"
        width="34%"
        type="text/html"
      />
      External content
    </div>
  </div>
</template>

<script>
export default {
  data() {
    return {
      entries: [],
      parsedDate: [],
      searchStatus: false
    };
  },
  computed: {},
  created: function() {
    // note - uses proxy
    fetch("http://localhost:8080/all/cache")
      .then(response => response.json())
      .then(data => {
        console.log("retrieved data");
        this.entries = data;
        return data;
      })
      .then(data => {
        let parsed = data.map(e => Date.parse(e.cvDate));
        let maxVal = Math.max(...parsed);
        let minVal = Math.min(...parsed);
        this.parsedDate = this.entries.map(e => ({
          entryID: e.cvForeignID,
          date: e.cvDate,
          pageTitle: e.cvContent,
          url: e.cvUrl,
          pDate: ((Date.parse(e.cvDate) - minVal) / (maxVal - minVal)) * 100
        }));
        console.log(this.parsedDate);
      });
  },
  mounted: function() {},
  methods: {
    timelineClick(e, url) {
      console.log("Clicked timeline");
      console.log(e);
      window.open(url);
    },
    drag(e) {
      console.log("drag " + e);
    },
    searchOver(e, value) {
      this.searchStatus = value;
      console.log(value);
    }
  },

  watch: {}
};
</script>

<style>
.searchdiv {
  width: 100%;
  margin-bottom: 40px;
  margin-top: 40px;
  font-size: 2em;
}

.searchinput {
  width: 90%;
}

.card {
  /* box-shadow: 10px, 10px grey; */
  display: block;
  box-shadow: 0px 5px 10px 0px rgba(0, 0, 0, 0.2),
    0px 4px 4px 0px rgba(0, 0, 0, 0.15), 0px 2px 2px 0px rgba(0, 0, 0, 0.2);
  background-color: #eeeeee;
  border-radius: 10px;
  width: 66%;
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

.tooltip {
  font-family: "arial";
  font-size: 1em;
  background: #eeeeeee;
  box-shadow: 0 5px 10px 0px rgba(0, 0, 0, 0.2);
  border-radius: 5px;
  border-color: #000000;
  &.popover {
    .popover-arrow {
      border-color: #000000;
      height: 20px;
      border-style: solid;
      margin: 10px;
    }
  }
}

.timeline {
  box-shadow: 0 5px 10px 0px rgba(0, 0, 0, 0.2);
  border-color: #000000;
  border-width: 10px;
  border-radius: 5px;
  width: 100%;
}

// https://codepen.io/JTBennett/pen/BjpXRo
.fade-in {
  animation: fadeIn ease 500ms;
  -webkit-animation: fadeIn ease 500ms;
  -moz-animation: fadeIn ease 500ms;
  -o-animation: fadeIn ease 500ms;
  -ms-animation: fadeIn ease 500ms;
}

@keyframes fadeIn {
  0% {
    opacity: 0;
  }
  100% {
    opacity: 1;
  }
}

@-moz-keyframes fadeIn {
  0% {
    opacity: 0;
  }
  100% {
    opacity: 1;
  }
}

@-webkit-keyframes fadeIn {
  0% {
    opacity: 0;
  }
  100% {
    opacity: 1;
  }
}

@-o-keyframes fadeIn {
  0% {
    opacity: 0;
  }
  100% {
    opacity: 1;
  }
}

@-ms-keyframes fadeIn {
  0% {
    opacity: 0;
  }
  100% {
    opacity: 1;
  }
}
</style>
