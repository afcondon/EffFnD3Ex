/* global exports */
"use strict";

// module Graphics.D3.Examples.ForceLayout1

exports.setFixed    = setFixed
exports.unsetFixed  = unsetFixed

function unsetFixed(d) {
  return function() {
    d.fixed = false;
  }
}

function setFixed(d) {
  return function() {
    d.fixed = true;
  }
}
