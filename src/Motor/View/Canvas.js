"use strict";

exports.jsResetCanvas = function(canvas) {
  return function() {
    canvas.width = canvas.width;
  };
};

exports.bitmapWidth = function(bm) {
  return function() {
    return bm.width;
  };
};


exports.bitmapHeight = function(bm) {
  return function() {
    return bm.height;
  };
};


exports.jsCreateCanvas = function(label) {
  return function() {
    return document.createElement(label);
  };
};


exports.setProp = function(a) {
  return function (b) {
    return function (c) {
      return function() {
        a[b] = c;
      };
    };
  };
};

exports.getProp = function(a) {
  return function (b) {
    return function() {
      return a[b].toString();
    };
  };
};
