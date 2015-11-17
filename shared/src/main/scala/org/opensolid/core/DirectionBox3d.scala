/*******************************************************************************
*                                                                              *
*  OpenSolid is a generic library for the representation and manipulation of   *
*  geometric objects such as points, curves, surfaces, and volumes.            *
*                                                                              *
*  Copyright 2007-2015 by Ian Mackenzie                                        *
*  ian.e.mackenzie@gmail.com                                                   *
*                                                                              *
*  This Source Code Form is subject to the terms of the Mozilla Public         *
*  License, v. 2.0. If a copy of the MPL was not distributed with this file,   *
*  you can obtain one at http://mozilla.org/MPL/2.0/.                          *
*                                                                              *
*******************************************************************************/

package org.opensolid.core

final case class DirectionBox3d(x: Interval, y: Interval, z: Interval) {
  def components: Array[Interval] = Array(x, y, z)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ =>
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds for DirectionBox3d")
  }

  def vector: VectorBox3d = VectorBox3d(x, y, z)

  def unary_- : DirectionBox3d = DirectionBox3d(-x, -y, -z)

  def *(sign: Sign): DirectionBox3d = DirectionBox3d(x * sign, y * sign, z * sign)

  def *(value: Double): VectorBox3d = VectorBox3d(x * value, y * value, z * value)

  def *(interval: Interval): VectorBox3d = VectorBox3d(x * interval, y * interval, z * interval)

  def /(value: Double): VectorBox3d = VectorBox3d(x / value, y / value, z / value)

  def /(interval: Interval): VectorBox3d = VectorBox3d(x / interval, y / interval, z / interval)

  def dot(vector: Vector3d): Interval = x * vector.x + y * vector.y + z * vector.z

  def dot(vectorBox: VectorBox3d): Interval = x * vectorBox.x + y * vectorBox.y + z * vectorBox.z

  def dot(direction: Direction3d): Interval = x * direction.x + y * direction.y + z * direction.z

  def dot(that: DirectionBox3d): Interval = this.x * that.x + this.y * that.y + this.z * that.z

  def cross(vector: Vector3d): VectorBox3d = this.vector.cross(vector)

  def cross(vectorBox: VectorBox3d): VectorBox3d = vector.cross(vectorBox)

  def cross(direction: Direction3d): VectorBox3d = vector.cross(direction.vector)

  def cross(directionBox: DirectionBox3d): VectorBox3d = vector.cross(directionBox.vector)
}

object DirectionBox3d {
  def apply(direction: Direction3d): DirectionBox3d =
    DirectionBox3d(Interval(direction.x), Interval(direction.y), Interval(direction.z))

  def fromComponents(components: Seq[Interval]): DirectionBox3d = components match {
    case Seq(x, y, z) => DirectionBox3d(x, y, z)
    case _ => throw new IllegalArgumentException("DirectionBox3d requires 3 components")
  }

  val None = DirectionBox3d(Interval.Empty, Interval.Empty, Interval.Empty)
}
