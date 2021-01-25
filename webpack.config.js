const { resolve } = require("path");
const webpack = require("webpack");

const {
  ENV,
  HTTP_PROVIDER_URL,
  DAI_CONTRACT_ADDRESS,
  SMOKE_SIGNAL_CONTRACT_ADDRESS,
  START_SCAN_BLOCK,
} = process.env;

if (
  [
    HTTP_PROVIDER_URL,
    DAI_CONTRACT_ADDRESS,
    SMOKE_SIGNAL_CONTRACT_ADDRESS,
    START_SCAN_BLOCK,
  ].some((x) => !x)
) {
  throw "Missing environment variable.";
}

const publicFolder = resolve("./public");

const isProd = ENV === "production";

const webpackLoader = {
  loader: "elm-webpack-loader",
  options: {
    debug: false,
    optimize: isProd,
    cwd: __dirname,
  },
};

const webpackPlugins = isProd
  ? [webpackLoader]
  : [{ loader: "elm-hot-webpack-loader" }, webpackLoader];

const mode = isProd ? "production" : "development";

module.exports = {
  mode,
  entry: "./src/index.js",
  devServer: {
    publicPath: "/",
    contentBase: publicFolder,
    port: 8000,
    hotOnly: true,
  },
  output: {
    publicPath: "/",
    path: publicFolder,
    filename: "bundle.js",
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: webpackPlugins,
      },
    ],
  },
  plugins: [
    new webpack.NoEmitOnErrorsPlugin(),
    new webpack.DefinePlugin({
      HTTP_PROVIDER_URL: JSON.stringify(HTTP_PROVIDER_URL),
      DAI_CONTRACT_ADDRESS: JSON.stringify(DAI_CONTRACT_ADDRESS),
      SMOKE_SIGNAL_CONTRACT_ADDRESS: JSON.stringify(
        SMOKE_SIGNAL_CONTRACT_ADDRESS
      ),
      // No stringify needed for an integer.
      START_SCAN_BLOCK,
    }),
  ],
};
