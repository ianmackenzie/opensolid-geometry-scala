"use strict";

const opensolid = require("..");

const Interval = opensolid.Interval;
const Point2d = opensolid.Point2d;
const Handedness = opensolid.Handedness;
const Sign = opensolid.Sign;
const Vector2d = opensolid.Vector2d;
const UnitVector2d = opensolid.UnitVector2d;

require("../arithmetic")
require("../comparisons")

console.log("=== interval ===");
const interval = Interval(2, 3);
console.log(interval.median);
console.log((-4).times(interval).toString());

console.log("=== singleton ===");
const singleton = Interval.singleton(5);
console.log(singleton.lowerBound);
console.log(singleton.upperBound);

console.log("=== comparisons ===");
const tiny = 1e-14;
const small = 1e-8;
console.log(tiny.isZero());
console.log(small.isZero());
console.log(small.isZero(1e-6));

console.log("=== handedness and sign ===");
console.log(Handedness.RIGHT_HANDED instanceof Handedness);
console.log(Handedness.RIGHT_HANDED instanceof Sign);
console.log(Sign.POSITIVE instanceof Handedness);
console.log(Sign.POSITIVE instanceof Sign);

console.log("=== unit vectors ===");
const e0 = UnitVector2d(1, 0);
console.log(e0 instanceof UnitVector2d);
console.log(e0 instanceof Vector2d);
console.log(e0.squaredLength);
console.log(e0.normalized.toString());
