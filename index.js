'use strict';

require('./main.css');
var icons = require('./icons.svg');
// Require index.html so it gets copied to dist
console.log(icons);
require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// The third value on embed are the initial values for incomming ports into Elm
var app = Elm.Main.embed(mountNode);
