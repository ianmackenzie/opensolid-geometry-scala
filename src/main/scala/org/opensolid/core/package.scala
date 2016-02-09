////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  OpenSolid is a generic library for the representation and manipulation    //
//  of geometric objects such as points, curves, surfaces, and volumes.       //
//                                                                            //
//  Copyright 2007-2015 by Ian Mackenzie                                      //
//  ian.e.mackenzie@gmail.com                                                 //
//                                                                            //
//  This Source Code Form is subject to the terms of the Mozilla Public       //
//  License, v. 2.0. If a copy of the MPL was not distributed with this file, //
//  you can obtain one at http://mozilla.org/MPL/2.0/.                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

package org.opensolid

import scala.math

/** OpenSolid is a 2D/3D geometry library for working with objects such as points, triangles,
  * curves, surfaces and solid bodies. It provides support for operations such as geometric
  * transformations, spatial searches, intersection calculations and meshing.
  *
  * All OpenSolid objects are immutable pure values; all transformations result in a copy of the
  * original object. All functions are pure (no side effects) and thread safe.
  *
  * === Units handling ===
  *
  * OpenSolid has no explicit tracking of units; in general it is assumed that all numbers are in
  * meters or radians. A variety of extensions have been added to the `Double` class to provide
  * convenient unit conversions; function names starting with 'in' provide conversions from a value
  * in meters (or radians) to one in another unit, and function names not starting with 'in' convert
  * from some unit to meters (or radians). Note that this means `meters` and `inMeters` (and
  * `radians` and `inRadians`) are no-ops, but are provided to make some expressions more clear.
  * For example:
  * {{{
  * 1.millimeters // 0.001
  * 1.millimeters.inMeters // 0.001
  * 1.meters // 1.0
  * 1.meters.inMillimeters // 1000.0
  * 1.feet // 0.3048
  * 1.feet.inMeters // 0.3048
  * 1.inFeet // 3.281
  * 1.meters.inFeet // 3.281
  * 180.degrees // 3.142, Pi radians
  * 1.radians.inDegrees // 57.296
  * }}}
  * In general, use an expression such as `2.5.feet` at the very beginning of a calculation to
  * convert into meters, do all calculations in meters, then use `result.inFeet` to convert back to
  * feet at the end for output. Use expressions such as `45.degrees` to pass fixed rotation angles
  * to functions expecting values in radians.
  *
  * === Naming conventions ===
  *
  * OpenSolid uses standard Scala naming conventions: values and functions are camelCase, types and
  * constants (such as `Point3d.Origin`) are PascalCase. Other than well-established function names
  * such as `equals` and `contains` (and, for symmetry, `overlaps`), functions returning Boolean
  * results start with 'is' or 'has' (e.g. `point.isOn(plane, tolerance)`).
  *
  * Functions transforming an object usually belong to the object being transformed, so for example
  * one uses `point.projectedOnto(plane)` instead of something like `plane.project(point)`.
  * In general, names have been chosen to make entire expressions read as naturally as possible as
  * 'noun phrases', for example `if (bounds.contains(lineSegment.rotatedAbout(axis, angle)) ...`.
  *
  * === Transformations framework ===
  *
  * Many OpenSolid classes inherit from the `Transformable2d` or `Transformable3d` traits (or for
  * vector-like classes, `VectorTransformable2d` or `VectorTransformable3d`). These provide support
  * for the common transformations such as translations, rotations, and mirrors, as well as
  * transformations between local and global coordinates.
  *
  * TODO
  *
  * === Bounds types ===
  *
  * TODO
  *
  * === Operators ===
  *
  * TODO (mention Java-friendly versions)
  *
  * === Tuples ===
  *
  * TODO
  */
package object core {
  implicit class DoubleExtensions(val value: Double) extends AnyVal {
    def isZero(tolerance: Double): Boolean =
      value >= -tolerance && value <= tolerance

    def isNotZero(tolerance: Double): Boolean =
      value < -tolerance || value > tolerance

    def meters: Double =
      value

    def inMeters: Double =
      value

    def centimeters: Double =
      value * 1e-2

    def inCentimeters: Double =
      value * 1e2

    def millimeters: Double =
      value * 1e-3

    def inMillimeters: Double =
      value * 1e3

    def microns: Double =
      value * 1e-6

    def inMicrons: Double =
      value * 1e6

    def kilometers: Double =
      value * 1e3

    def inKilometers: Double =
      value * 1e-3

    def inches: Double =
      value * 0.0254

    def inInches: Double =
      value / 0.0254

    def feet: Double =
      value * 0.3048

    def inFeet: Double =
      value / 0.3048

    def thou: Double =
      value * 0.0000254

    def inThou: Double =
      value / 0.0000254

    def yards: Double =
      value * 0.9144

    def inYards: Double =
      value / 0.9144

    def miles: Double =
      value * 1609.344

    def inMiles: Double =
      value / 1609.344

    def radians: Double =
      value

    def inRadians: Double =
      value

    def degrees: Double =
      math.toRadians(value)

    def inDegrees: Double =
      math.toDegrees(value)

    def +(interval: Interval): Interval =
      interval + value

    def -(interval: Interval): Interval =
      Interval(value - interval.upperBound, value - interval.lowerBound)

    def *(interval: Interval): Interval =
      interval * value

    def /(interval: Interval): Interval = {
      if (interval.isEmpty) {
        Interval.Empty
      } else if (interval.lowerBound > 0.0) {
        if (value >= 0.0) {
          Interval(value / interval.upperBound, value / interval.lowerBound)
        } else {
          Interval(value / interval.lowerBound, value / interval.upperBound)
        }
      } else if (interval.upperBound < 0.0) {
        if (value >= 0.0) {
          Interval(value / interval.upperBound, value / interval.lowerBound)
        } else {
          Interval(value / interval.lowerBound, value / interval.upperBound)
        }
      } else if (value == 0.0) {
        Interval.Zero
      } else {
        Interval.Whole
      }
    }

    def *(vector: Vector2d): Vector2d =
      vector * value

    def *(vector: Vector3d): Vector3d =
      vector * value

    def *(direction: Direction2d): Vector2d =
      direction * value

    def *(direction: Direction3d): Vector3d =
      direction * value

    def +[P](scalarExpression: ScalarExpression[P]): ScalarExpression[P] =
      ScalarExpression.Constant[P](value) + scalarExpression

    def -[P](scalarExpression: ScalarExpression[P]): ScalarExpression[P] =
      ScalarExpression.Constant[P](value) - scalarExpression

    def *[P](scalarExpression: ScalarExpression[P]): ScalarExpression[P] =
      ScalarExpression.Constant[P](value) * scalarExpression

    def /[P](scalarExpression: ScalarExpression[P]): ScalarExpression[P] =
      ScalarExpression.Constant[P](value) / scalarExpression

    def *[P](vectorExpression: VectorExpression2d[P]): VectorExpression2d[P] =
      ScalarExpression.Constant[P](value) * vectorExpression

    def hull(that: Double): Interval =
      Interval(value.min(that), value.max(that))

    def hull(interval: Interval): Interval =
      interval.hull(value)
  }

  implicit class Tuple2Extensions[T](val tuple: (T, T)) extends AnyVal {
    def first: T =
      tuple match { case (first, second) => first }

    def second: T =
      tuple match { case (first, second) => second }

    def map[U](function: (T) => U): (U, U) =
      (function(first), function(second))

    def foreach(function: (T) => Unit): Unit = {
      function(first)
      function(second)
    }

    def apply(index: Int): T = index match {
      case 0 => first
      case 1 => second
      case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for a pair")
    }

    def count(function: (T) => Boolean): Int =
      (if (function(first)) 1 else 0) + (if (function(second)) 1 else 0)

    def reduce[R](function: (T, T) => R): R =
      function(first, second)

    def reverse: (T, T) =
      (second, first)
  }

  implicit class Tuple3Extensions[T](val tuple: (T, T, T)) extends AnyVal {
    def first: T =
      tuple match { case (first, second, third) => first }

    def second: T =
      tuple match { case (first, second, third) => second }

    def third: T =
      tuple match { case (first, second, third) => third }

    def map[U](function: (T) => U): (U, U, U) =
      (function(first), function(second), function(third))

    def foreach(function: (T) => Unit): Unit = {
      function(first)
      function(second)
      function(third)
    }

    def apply(index: Int): T = index match {
      case 0 => first
      case 1 => second
      case 2 => third
      case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for a triple")
    }

    def count(function: (T) => Boolean): Int =
      (if (function(first)) 1 else 0) +
      (if (function(second)) 1 else 0) +
      (if (function(third)) 1 else 0)

    def reduce[R](function: (T, T, T) => R): R =
      function(first, second, third)

    def reverse: (T, T, T) =
      (third, second, first)
  }
}
