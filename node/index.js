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

module.exports = opensolid;
