module.exports = {
  configureWebpack: {
    devtool: "source-map",
    externals: {
      bokeh: "Bokeh",
    },
  },
  transpileDependencies: ["vuetify"],
};
