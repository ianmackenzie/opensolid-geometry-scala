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

package org.opensolid.core

class Scalar(val value: Double) extends AnyVal {
  def isZero(tolerance: Double): Boolean =
    value.isZero(tolerance)

  def isNotZero(tolerance: Double): Boolean =
    value.isNotZero(tolerance)

  def meters: Double =
    value.meters

  def inMeters: Double =
    value.inMeters

  def centimeters: Double =
    value.centimeters

  def inCentimeters: Double =
    value.inCentimeters

  def millimeters: Double =
    value.millimeters

  def inMillimeters: Double =
    value.inMillimeters

  def microns: Double =
    value.microns

  def inMicrons: Double =
    value.inMicrons

  def kilometers: Double =
    value.kilometers

  def inKilometers: Double =
    value.inKilometers

  def inches: Double =
    value.inches

  def inInches: Double =
    value.inInches

  def feet: Double =
    value.feet

  def inFeet: Double =
    value.inFeet

  def thou: Double =
    value.thou

  def inThou: Double =
    value.inThou

  def yards: Double =
    value.yards

  def inYards: Double =
    value.inYards

  def miles: Double =
    value.miles

  def inMiles: Double =
    value.inMiles

  def radians: Double =
    value.radians

  def inRadians: Double =
    value.inRadians

  def degrees: Double =
    value.degrees

  def inDegrees: Double =
    value.inDegrees

  def plus(interval: Interval): Interval =
    value + interval

  def minus(interval: Interval): Interval =
    value - interval

  def times(interval: Interval): Interval =
    value * interval

  def dividedBy(interval: Interval): Interval =
    value / interval

  def times(vector: Vector2d): Vector2d =
    value * vector

  def times(vector: Vector3d): Vector3d =
    value * vector

  def times(direction: Direction2d): Vector2d =
    value * direction

  def times(direction: Direction3d): Vector3d =
    value * direction

  def plus[P](scalarExpression: ScalarExpression[P]): ScalarExpression[P] =
    value + scalarExpression

  def minus[P](scalarExpression: ScalarExpression[P]): ScalarExpression[P] =
    value - scalarExpression

  def times[P](scalarExpression: ScalarExpression[P]): ScalarExpression[P] =
    value * scalarExpression

  def dividedBy[P](scalarExpression: ScalarExpression[P]): ScalarExpression[P] =
    value / scalarExpression

  def times[P](vectorExpression: VectorExpression2d[P]): VectorExpression2d[P] =
    value * vectorExpression

  def times[P](vectorExpression: VectorExpression3d[P]): VectorExpression3d[P] =
    value * vectorExpression

  def hull(that: Double): Interval =
    value.hull(that)

  def hull(interval: Interval): Interval =
    value.hull(interval)
}
