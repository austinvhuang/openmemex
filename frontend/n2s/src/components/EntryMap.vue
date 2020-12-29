<template>
  <div>
    Hi
    <svg>
      <circle
        cx="50%"
        cy="50%"
        r="4"
        stroke="#666666"
        stroke-width="1"
        fill="#CCCCCC"
        opacity="1.0"
      />
    </svg>

    <div id="viz" class="viz-style">Hello</div>
    <div class="edit">
      <textarea v-model="vlobj"></textarea>
    </div>
  </div>
</template>

<script>
import embed from "vega-embed";
export default {
  data() {
    return {
      vlobj: {
        values: [
          { a: "A", b: 28 },
          { a: "B", b: 55 },
          { a: "C", b: 43 },
          { a: "D", b: 91 },
          { a: "E", b: 81 },
          { a: "F", b: 53 },
          { a: "G", b: 19 },
          { a: "H", b: 87 },
          { a: "I", b: 52 },
        ],
        encoding: {
          x: { field: "a", type: "ordinal" },
          y: { field: "b", type: "quantitative" },
        },
      },
      watch: {
        vlobj(v) {
          if (v) this.draw();
        },
      },
      methods: {
        draw() {
          let vlobj = this.vlobj;
          vlobj.width = "container";
          vlobj.height = "container";
          embed("#viz", vlobj, { actions: false });
        },
      },
      mounted() {
        embed("#viz", this.vlobj, { actions: false });
      },
    };
  },
};
</script>

<style>
.viz-style {
  background-color: "red";
}
</style>