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

abstract class Bounds[T] {
  def component(bounds: T, index: Int): Interval

  def overlaps(firstBounds: T, secondBounds: T, tolerance: Double): Boolean

  def contains(firstBounds: T, secondBounds: T, tolerance: Double): Boolean

  def bisected(bounds: T, index: Int): (T, T)

  def hull(firstBounds: T, secondBounds: T): T

  def hasLesserMedian(firstBounds: T, secondBounds: T, index: Int): Boolean = {
    val difference = component(firstBounds, index) - component(secondBounds, index)
    difference.upperBound < -difference.lowerBound
  }

  def hasEqualMedian(firstBounds: T, secondBounds: T, index: Int): Boolean = {
    val difference = component(firstBounds, index) - component(secondBounds, index)
    difference.upperBound == -difference.lowerBound
  }

  def hasGreaterMedian(firstBounds: T, secondBounds: T, index: Int): Boolean = {
    val difference = component(firstBounds, index) - component(secondBounds, index)
    difference.upperBound > -difference.lowerBound
  }

  val NumDimensions: Int

  val Empty: T
}
