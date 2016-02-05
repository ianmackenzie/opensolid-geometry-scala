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

object Meters {
  def fromCentimeters(value: Double): Double =
    value.centimeters

  def toCentimeters(value: Double): Double =
    value.inCentimeters

  def fromMillimeters(value: Double): Double =
    value.millimeters

  def toMillimeters(value: Double): Double =
    value.inMillimeters

  def fromMicrons(value: Double): Double =
    value.microns

  def toMicrons(value: Double): Double =
    value.inMicrons

  def fromKilometers(value: Double): Double =
    value.kilometers

  def toKilometers(value: Double): Double =
    value.inKilometers

  def fromInches(value: Double): Double =
    value.inches

  def toInches(value: Double): Double =
    value.inInches

  def fromFeet(value: Double): Double =
    value.feet

  def toFeet(value: Double): Double =
    value.inFeet

  def fromThou(value: Double): Double =
    value.thou

  def toThou(value: Double): Double =
    value.inThou

  def fromYards(value: Double): Double =
    value.yards

  def toYards(value: Double): Double =
    value.inYards

  def fromMiles(value: Double): Double =
    value.miles

  def toMiles(value: Double): Double =
    value.inMiles
}
