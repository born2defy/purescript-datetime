/* global exports */
"use strict";

// module ExistsR

exports.mkExistsR = function (fa) {
  return fa;
};

exports.runExistsR = function (f) {
  return function (fa) {
    return f(fa);
  };
};
