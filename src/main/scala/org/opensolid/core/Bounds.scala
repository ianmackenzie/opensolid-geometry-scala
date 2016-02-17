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

abstract class Bounds[B <: Bounds[B]] {
  def component(index: Int): Interval

  def overlaps(that: B, tolerance: Double): Boolean

  def contains(that: B, tolerance: Double): Boolean

  def bisected(index: Int): (B, B)

  def hull(that: B): B

  def hasLesserMedianThan(that: B, index: Int): Boolean = {
    val difference = component(index) - that.component(index)
    difference.upperBound < -difference.lowerBound
  }

  def hasEqualMedianTo(that: B, index: Int): Boolean = {
    val difference = component(index) - that.component(index)
    difference.upperBound == -difference.lowerBound
  }

  def hasGreaterMedianThan(that: B, index: Int): Boolean = {
    val difference = component(index) - that.component(index)
    difference.upperBound > -difference.lowerBound
  }
}
