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

final case class DirectionBounds2d(vectorBounds: VectorBounds2d) {
  def this(x: Interval, y: Interval) = this(VectorBounds2d(x, y))

  def this(components: (Interval, Interval)) = this(components.first, components.second)

  def x: Interval = vectorBounds.x

  def y: Interval = vectorBounds.y

  def components: (Interval, Interval) = vectorBounds.components

  def component(index: Int): Interval = vectorBounds.component(index)

  def unary_- : DirectionBounds2d = DirectionBounds2d(-vectorBounds)

  def *(value: Double): VectorBounds2d = vectorBounds * value

  def *(interval: Interval): VectorBounds2d = vectorBounds * interval

  def /(value: Double): VectorBounds2d = vectorBounds / value

  def /(interval: Interval): VectorBounds2d = vectorBounds / interval
}

object DirectionBounds2d {
  def apply(x: Interval, y: Interval): DirectionBounds2d = new DirectionBounds2d(x, y)

  def apply(components: (Interval, Interval)): DirectionBounds2d = new DirectionBounds2d(components)

  def singleton(direction: Direction2d): DirectionBounds2d =
    DirectionBounds2d(VectorBounds2d.singleton(direction.vector))

  val Empty = DirectionBounds2d(Interval.Empty, Interval.Empty)
}
