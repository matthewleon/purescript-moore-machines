"use strict";

exports.mkStep = function (s) {
  return function (o) {
    return function () {
      return {halted: false, s: s, o: o};
    };
  };
};

exports.halted = function (step) {
  return function () {
    return step.halted;
  };
};

exports.output = function (step) {
  return function () {
    return step.output;
  };
};
