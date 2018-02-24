"use strict";

var Connect = require('uport-connect').Connect;


exports._newConnect = function(appName, opts) {
  return new Connect(appName, opts)
};


exports._getProvider = function(c) {
  console.log("_getProvider")
  return c.getProvider()
};

