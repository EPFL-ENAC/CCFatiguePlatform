module.exports = {
  configureWebpack: {
    devtool: "source-map",
  },
  transpileDependencies: ["vuetify"],
  devServer: {
    proxy: {
      "/api": {
        target: "http://localhost:8000",
        changeOrigin: true,
        pathRewrite: { "^/api": "" },
        logLevel: "debug",
      },
    },
  },
};
