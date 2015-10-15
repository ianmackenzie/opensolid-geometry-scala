"use strict";

let opensolid = require("./lib/opensolid-opt");

// Merge static functions and constants into properties of constructor functions
const staticMembersSuffix = "$StaticMembers";
const suffixLength = staticMembersSuffix.length;
for (let exportedName in opensolid) {
  if (exportedName.endsWith(staticMembersSuffix)) {
    let staticMembers = opensolid[exportedName]();
    let className = exportedName.substring(0, exportedName.length - suffixLength);
    let destinationClass = opensolid[className];
    if (destinationClass === undefined) {
      console.log("Error: " + className + " is not defined");
    } else {
      for (let memberName in staticMembers) {
        if (destinationClass[memberName] === undefined) {
          destinationClass[memberName] = staticMembers[memberName];
        }
      }
    }
  }
}

// Define function for (optionally) adding tolerant comparison functions to
// JavaScript numbers
opensolid.addComparisonFunctionsTo = function (numberClass) {
  numberClass.prototype.isLessThanZero = function (precision) {
    if (precision === undefined) precision = 1e-12;
    return this < -precision;
  };
  numberClass.prototype.isLessThanOrEqualToZero = function (precision) {
    if (precision === undefined) precision = 1e-12;
    return this <= precision;
  };
  numberClass.prototype.isZero = function (precision) {
    if (precision === undefined) precision = 1e-12;
    return this >= -precision && this <= precision;
  };
  numberClass.prototype.isGreaterThanOrEqualToZero = function (precision) {
    if (precision === undefined) precision = 1e-12;
    return this >= -precision;
  };
  numberClass.prototype.isGreaterThanZero = function (precision) {
    if (precision === undefined) precision = 1e-12;
    return this > precision;
  };
};

// Define function for (optionally) adding interval/vector arithmetic functions
// to JavaScript numbers
opensolid.addArithmeticFunctionsTo = function(numberClass) {
  numberClass.prototype.plus = function(other) {
    return opensolid.ScalarOperations().sum(this, other);
  }
  numberClass.prototype.minus = function(other) {
    return opensolid.ScalarOperations().difference(this, other);
  }
  numberClass.prototype.times = function(other) {
    return opensolid.ScalarOperations().product(this, other);
  }
  numberClass.prototype.dividedBy = function(other) {
    return opensolid.ScalarOperations().quotient(this, other);
  }
};

module.exports = opensolid;
