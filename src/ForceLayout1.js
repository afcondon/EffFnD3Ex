/* global exports */
"use strict";

// module Graphics.D3.Examples.ForceLayout1

// foreign function proof of concept for fulfillment of callbacks
// obviously not what we want since the WHOLE POINT of moving to EffFn was to be able to write
// the callbacks in Purescript

exports.customDoubleClickHandler = dblclick2
exports.customDragStartHandler   = drag

function drag(d) {
  var foo = d.index;
  return function() {
    d3.selectAll(".node")
    .filter(function(n) {
      return n.index == foo;
    })
    .classed('fixed', d.fixed = true);
  }
}



function dblclick(d) {
  var that = d3.select(this); // cache the selection in the closure for the inner function that is actually called
  return function() {
    console.log("this is the inner function that is called in final stage of mkEffFn1");
    that.classed("fixed", d.fixed = false);
  }
}


function dblclick2(d) {
  var foo = d.index;
  return function() {
    d3.selectAll(".node")
    .filter(function(n) {
      return n.index == foo;
    })
    .classed('fixed', d.fixed = false);
  }
}
