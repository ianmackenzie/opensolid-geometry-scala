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

final case class DirectionBox3d(vectorBox: VectorBox3d) {
  def x: Interval = vectorBox.x

  def y: Interval = vectorBox.y

  def z: Interval = vectorBox.z

  def components: Array[Interval] = vectorBox.components

  def component(index: Int): Interval = vectorBox.component(index)

  def unary_- : DirectionBox3d = DirectionBox3d(-vectorBox)

  def *(sign: Sign): DirectionBox3d = DirectionBox3d(vectorBox * sign)

  def *(value: Double): VectorBox3d = vectorBox * value

  def *(interval: Interval): VectorBox3d = vectorBox * interval

  def /(value: Double): VectorBox3d = vectorBox / value

  def /(interval: Interval): VectorBox3d = vectorBox / interval

  def dot(vector: Vector3d): Interval = vectorBox.dot(vector)

  def dot(vectorBox: VectorBox3d): Interval = this.vectorBox.dot(vectorBox)

  def dot(direction: Direction3d): Interval = vectorBox.dot(direction.vector)

  def dot(that: DirectionBox3d): Interval = this.vectorBox.dot(that.vectorBox)

  def cross(vector: Vector3d): VectorBox3d = vectorBox.cross(vector)

  def cross(vectorBox: VectorBox3d): VectorBox3d = this.vectorBox.cross(vectorBox)

  def cross(direction: Direction3d): VectorBox3d = vectorBox.cross(direction.vector)

  def cross(that: DirectionBox3d): VectorBox3d = this.vectorBox.cross(that.vectorBox)
}

object DirectionBox3d {
  def apply(x: Interval, y: Interval, z: Interval): DirectionBox3d =
    DirectionBox3d(VectorBox3d(x, y, z))

  def apply(direction: Direction3d): DirectionBox3d =
    DirectionBox3d(Interval(direction.x), Interval(direction.y), Interval(direction.z))

  def fromComponents(components: Seq[Interval]): DirectionBox3d = components match {
    case Seq(x, y, z) => DirectionBox3d(x, y, z)
    case _ => throw new IllegalArgumentException("DirectionBox3d requires 3 components")
  }

  val Empty = DirectionBox3d(Interval.Empty, Interval.Empty, Interval.Empty)
}
