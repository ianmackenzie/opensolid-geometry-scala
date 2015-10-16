"use strict";

let opensolid = require("..");

Number.prototype.plus = function(other) {
  return opensolid.ScalarOperations().sum(this, other);
};

Number.prototype.minus = function(other) {
  return opensolid.ScalarOperations().difference(this, other);
};

Number.prototype.times = function(other) {
  return opensolid.ScalarOperations().product(this, other);
};

Number.prototype.dividedBy = function(other) {
  return opensolid.ScalarOperations().quotient(this, other);
};
