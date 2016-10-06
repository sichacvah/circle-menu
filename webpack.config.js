var path = require("path");
var IconsPlugin = require('icons-loader/IconsPlugin');

const RUN_TIMESTAMP = Math.round(Date.now() / 1000);

module.exports = {
  entry: {
    app: [
      './index.js'
    ]
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: '[name].js',
  },


  module: {
    loaders: [
      {
        test: /\.(css|scss)$/,
        loaders: [
          'style-loader',
          'css-loader',
        ]
      },
      {
        test:    /\.html$/,
        exclude: /node_modules/,
        loader:  'file?name=[name].[ext]',
      },
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader:  'elm-webpack',
      },
      {
        test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'url-loader?limit=10000&mimetype=application/font-woff',
      },
      {
        test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'file-loader',
      }
    ],

    noParse: /\.elm$/,
  },

  devServer: {
    inline: true,
    stats: { colors: true },
  },

};
