'use strict';

let opensolid = require('..');
let Interval = opensolid.Interval;
let Point2d = opensolid.Point2d;
let Handedness = opensolid.Handedness;
let Sign = opensolid.Sign;

opensolid.addTolerantComparisonsTo(Number);

console.log('=== interval ===');
let interval = Interval(2, 3);
console.log(interval.median);

console.log('=== singleton ===');
let singleton = Interval.singleton(5);
console.log(singleton.lowerBound);
console.log(singleton.upperBound);

console.log('=== comparisons ===');
let tiny = 1e-14;
let small = 1e-8;
console.log(tiny.isZero());
console.log(small.isZero());
console.log(small.isZero(1e-6));

console.log('=== handedness and sign ===')
console.log(Handedness.RIGHT_HANDED instanceof Handedness);
console.log(Handedness.RIGHT_HANDED instanceof Sign);
console.log(Sign.POSITIVE instanceof Handedness);
console.log(Sign.POSITIVE instanceof Sign);
