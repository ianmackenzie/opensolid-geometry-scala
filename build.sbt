enablePlugins(ScalaJSPlugin)

name := "OpenSolid"

scalaVersion := "2.11.7"

postLinkJSEnv := NodeJSEnv().value

scalaJSStage in Global := FastOptStage

artifactPath in (Compile, fullOptJS) := file("node/opensolid.js")

scalaJSOutputWrapper := (
  """
  'use strict';
  let __ScalaJSEnv = {
    exportsNamespace: exports
  };
  """,
  """
  const staticMembersSuffix = "_StaticMembers";
  const suffixLength = staticMembersSuffix.length;
  for (let exportedName in exports) {
    if (exportedName.endsWith(staticMembersSuffix)) {
      let staticMembers = exports[exportedName]();
      let className = exportedName.substring(0, exportedName.length - suffixLength);
      let destinationClass = exports[className];
      if (destinationClass !== undefined) {
        for (let memberName in staticMembers) {
          if (destinationClass[memberName] === undefined) {
            destinationClass[memberName] = staticMembers[memberName];
          }
        }
      }
    }
  }

  exports.addTolerantComparisonsTo = function (numberClass) {
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
    numberClass.prototype.isGreateThanZero = function (precision) {
      if (precision === undefined) precision = 1e-12;
      return this > precision;
    };
  }
  """
)
