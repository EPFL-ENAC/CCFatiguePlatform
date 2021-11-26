module.exports = {
  configureWebpack: {
    externals: {
      bokeh: "Bokeh",
    },
  },
  transpileDependencies: ["vuetify"],
};
