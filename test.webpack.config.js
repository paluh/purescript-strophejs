'use strict';

var BowerWebpackPlugin = require('bower-webpack-plugin');
var WebpackShellPlugin = require('webpack-shell-plugin');
var WrapperPlugin = require('wrapper-webpack-plugin');

var bowerWebpackPlugin = new BowerWebpackPlugin({moduleDirectories: ['./bower_components']});

var config = {
  entry: {
    'test': './output/Test.Main/index.js'
  },
  devtool: 'eval', //'source-map',
  output: {
    path: __dirname,
    library: '[name]',
    pathinfo: true,
    filename: '[name].bundle.js'
  },
  plugins: [
    bowerWebpackPlugin,
    new WrapperPlugin({footer: 'test.main()'}),
    new WebpackShellPlugin({onBuildExit: ['clear', 'phantomjs test.bundle.js']})
  ],
  module: {
    loaders: [{
      test: /\.purs$/,
      loader: "purs-loader",
      query: {
        src: [ 'bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs', 'test/**/*.purs' ],
        bundle: false,
        pscBundleArgs: {'main': 'Test.Main', 'module': 'Test.Main'},
        output: './output',
        psc: 'psc',
        pscIde: true,
        pscIdeArgs: {'port': 3333},
        watch: true
      }
    }, {
      test: /\.js$/,
      loader: 'babel-loader',
      exclude: []
    }]
  },
  resolve: {
    modules: [ 'node_modules', 'bower_components', 'output' ],
    extensions: [ '.purs', '.js'],
    plugins: [bowerWebpackPlugin]
  }
};

module.exports = config;
