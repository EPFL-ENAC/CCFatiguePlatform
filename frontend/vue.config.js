module.exports = {
  configureWebpack: {
    devtool: "source-map",
    watchOptions: {
      poll: 1000,
      aggregateTimeout: 300,
    },
  },
  transpileDependencies: ["vuetify"],
  devServer: {
    hot: true,
    liveReload: true,
    watchFiles: ["src/**/*"],
    client: {
      logging: "verbose",
      overlay: true,
    },
    proxy: {
      "/api": {
        target: "http://localhost:8000",
        changeOrigin: true,
        pathRewrite: { "^/api": "" },
        logLevel: "debug", // Most verbose logging
        onProxyReq(proxyReq, req, res) {
          console.log("Proxy request:", req.method, req.url);
          if (req.body) console.log("Request body:", req.body);
        },
        onProxyRes(proxyRes, req, res) {
          console.log("Proxy response:", proxyRes.statusCode, req.url);
        },
        onError(err, req, res) {
          console.error("Proxy error:", err);
        },
      },
    },
  },
};
