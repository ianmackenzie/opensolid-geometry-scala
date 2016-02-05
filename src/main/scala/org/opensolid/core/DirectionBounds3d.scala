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

final case class DirectionBounds3d(vectorBounds: VectorBounds3d) {
  def this(x: Interval, y: Interval, z: Interval) =
    this(VectorBounds3d(x, y, z))

  def x: Interval =
    vectorBounds.x

  def y: Interval =
    vectorBounds.y

  def z: Interval =
    vectorBounds.z

  def components: (Interval, Interval, Interval) =
    vectorBounds.components

  def component(index: Int): Interval =
    vectorBounds.component(index)

  def unary_- : DirectionBounds3d =
    DirectionBounds3d(-vectorBounds)

  def *(value: Double): VectorBounds3d =
    vectorBounds * value

  def *(interval: Interval): VectorBounds3d =
    vectorBounds * interval
}

object DirectionBounds3d {
  def apply(x: Interval, y: Interval, z: Interval): DirectionBounds3d =
    DirectionBounds3d(VectorBounds3d(x, y, z))

  def fromComponents(components: (Interval, Interval, Interval)): DirectionBounds3d =
    DirectionBounds3d(VectorBounds3d.fromComponents(components))

  def singleton(direction: Direction3d): DirectionBounds3d =
    DirectionBounds3d(VectorBounds3d.singleton(direction.vector))

  val Empty = DirectionBounds3d(Interval.Empty, Interval.Empty, Interval.Empty)
}
