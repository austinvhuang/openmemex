<template>
  <v-app>
    <v-app-bar
      app
      color="primary"
      dark
    >
      <div class="d-flex align-center">
        <v-img
          alt="Vuetify Logo"
          class="shrink mr-2"
          contain
          src="https://cdn.vuetifyjs.com/images/logos/vuetify-logo-dark.png"
          transition="scale-transition"
          width="40"
        />

        <h3>note2self.ai</h3>
      </div>

      <v-spacer></v-spacer>

    </v-app-bar>

    <v-main>
      <EntryCards/>
    </v-main>
  </v-app>
</template>

<script>
// import HelloWorld from './components/HelloWorld';
import EntryCards from './components/EntryCards';

export default {
  name: 'App',

  components: {
    EntryCards,
  },

  data: () => ({
    //
  }),
};
</script>
