"use strict";

Number.prototype.isLessThanZero = function (precision) {
  if (precision === undefined) precision = 1e-12;
  return this < -precision;
};

Number.prototype.isLessThanOrEqualToZero = function (precision) {
  if (precision === undefined) precision = 1e-12;
  return this <= precision;
};

Number.prototype.isZero = function (precision) {
  if (precision === undefined) precision = 1e-12;
  return this >= -precision && this <= precision;
};

Number.prototype.isGreaterThanOrEqualToZero = function (precision) {
  if (precision === undefined) precision = 1e-12;
  return this >= -precision;
};

Number.prototype.isGreaterThanZero = function (precision) {
  if (precision === undefined) precision = 1e-12;
  return this > precision;
};
