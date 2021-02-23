const { resolve } = require("path");
const webpack = require("webpack");

const {
  ENV,
  ETH_PROVIDER_URL,
  XDAI_PROVIDER_URL,
  START_SCAN_BLOCK,
} = process.env;

if ([ETH_PROVIDER_URL, XDAI_PROVIDER_URL, START_SCAN_BLOCK].some((x) => !x)) {
  throw "Missing environment variable(s).";
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
      {
        test: /\.(woff(2)?|ttf)$/,
        use: ["base64-inline-loader"],
      },
      {
        test: /\.css$/,
        use: ["style-loader", "css-loader"],
      },
    ],
  },
  plugins: [
    new webpack.NoEmitOnErrorsPlugin(),
    new webpack.DefinePlugin({
      ETH_PROVIDER_URL: JSON.stringify(ETH_PROVIDER_URL),
      XDAI_PROVIDER_URL: JSON.stringify(XDAI_PROVIDER_URL),
      // No stringify needed for an integer.
      START_SCAN_BLOCK,
    }),
  ],
};
