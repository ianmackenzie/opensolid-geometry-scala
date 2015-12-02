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

final case class DirectionBoundingBox3d(vectorBoundingBox: VectorBoundingBox3d) {
  def x: Interval = vectorBoundingBox.x

  def y: Interval = vectorBoundingBox.y

  def z: Interval = vectorBoundingBox.z

  def components: Array[Interval] = vectorBoundingBox.components

  def component(index: Int): Interval = vectorBoundingBox.component(index)

  def unary_- : DirectionBoundingBox3d = DirectionBoundingBox3d(-vectorBoundingBox)

  def *(sign: Sign): DirectionBoundingBox3d = DirectionBoundingBox3d(vectorBoundingBox * sign)

  def *(value: Double): VectorBoundingBox3d = vectorBoundingBox * value

  def *(interval: Interval): VectorBoundingBox3d = vectorBoundingBox * interval

  def /(value: Double): VectorBoundingBox3d = vectorBoundingBox / value

  def /(interval: Interval): VectorBoundingBox3d = vectorBoundingBox / interval

  def dot(vector: Vector3d): Interval = vectorBoundingBox.dot(vector)

  def dot(vectorBoundingBox: VectorBoundingBox3d): Interval =
    this.vectorBoundingBox.dot(vectorBoundingBox)

  def dot(direction: Direction3d): Interval = vectorBoundingBox.dot(direction.vector)

  def dot(that: DirectionBoundingBox3d): Interval =
    this.vectorBoundingBox.dot(that.vectorBoundingBox)

  def cross(vector: Vector3d): VectorBoundingBox3d = vectorBoundingBox.cross(vector)

  def cross(vectorBoundingBox: VectorBoundingBox3d): VectorBoundingBox3d =
    this.vectorBoundingBox.cross(vectorBoundingBox)

  def cross(direction: Direction3d): VectorBoundingBox3d = vectorBoundingBox.cross(direction.vector)

  def cross(that: DirectionBoundingBox3d): VectorBoundingBox3d =
    this.vectorBoundingBox.cross(that.vectorBoundingBox)
}

object DirectionBoundingBox3d {
  def apply(x: Interval, y: Interval, z: Interval): DirectionBoundingBox3d =
    DirectionBoundingBox3d(VectorBoundingBox3d(x, y, z))

  def apply(direction: Direction3d): DirectionBoundingBox3d =
    DirectionBoundingBox3d(Interval(direction.x), Interval(direction.y), Interval(direction.z))

  def fromComponents(components: Seq[Interval]): DirectionBoundingBox3d = components match {
    case Seq(x, y, z) => DirectionBoundingBox3d(x, y, z)
    case _ => throw new IllegalArgumentException("DirectionBoundingBox3d requires 3 components")
  }

  val Empty = DirectionBoundingBox3d(Interval.Empty, Interval.Empty, Interval.Empty)
}
