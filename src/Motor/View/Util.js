"use strict";

exports.innerWidth = function() {
  return window.innerWidth;
};

exports.innerHeight = function() {
  return window.innerHeight;
};

exports.addHead = function(doc) {
  return function(s) {
    return function() {
      var elm = document.createElement("h");
      // document.head.appendChild(elm);
      document.getElementsByTagName('head')[0].appendChild(elm);
      elm.outerHTML = s;
    };
  };
};

exports.hideCursor = function(doc) {
  return function() {
    var style = document.createElement("style");
    style.innerHTML = "canvas { cursor: none; }";
    // document.body.appendChild(elm);
    document.getElementsByTagName('body')[0].appendChild(style);
  };
};
