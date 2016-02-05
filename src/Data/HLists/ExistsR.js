/* global exports */
"use strict";

// module Data.HLists.ExistsR

exports.mkExistsR = function (fa) {
  return fa;
};

exports.runExistsR = function (f) {
  return function (fa) {
    return f(fa);
  };
};
