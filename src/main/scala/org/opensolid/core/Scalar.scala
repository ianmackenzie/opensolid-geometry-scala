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

object Scalar {
  def isZero(value: Double, tolerance: Double): Boolean = value.isZero(tolerance)

  def isNotZero(value: Double, tolerance: Double): Boolean = value.isNotZero(tolerance)

  def difference(value: Double, interval: Interval): Interval  = value - interval

  def quotient(value: Double, interval: Interval): Interval = value / interval

  def hull(firstValue: Double, secondValue: Double): Interval = firstValue.hull(secondValue)
}
