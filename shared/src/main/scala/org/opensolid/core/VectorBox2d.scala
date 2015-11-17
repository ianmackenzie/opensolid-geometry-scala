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

final case class VectorBox2d(x: Interval, y: Interval) {
  def components: Array[Interval] = Array(x, y)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for VectorBox2d")
  }

  def squaredLength: Interval = x * x + y * y

  def length: Interval = Interval.sqrt(squaredLength)

  def normalized: VectorBox2d = direction.vector

  def direction: DirectionBox2d = {
    if (this == VectorBox2d.Zero) {
      DirectionBox2d.None
    } else {
      val length = this.length
      DirectionBox2d(x / length, y / length)
    }
  }

  def unary_- : VectorBox2d = VectorBox2d(-x, -y)

  def +(vector: Vector2d): VectorBox2d = VectorBox2d(x + vector.x, y + vector.y)

  def +(that: VectorBox2d): VectorBox2d = VectorBox2d(this.x + that.x, this.y + that.y)

  def -(vector: Vector2d): VectorBox2d = VectorBox2d(x - vector.x, y - vector.y)

  def -(that: VectorBox2d): VectorBox2d = VectorBox2d(this.x - that.x, this.y - that.y)

  def *(sign: Sign): VectorBox2d = VectorBox2d(x * sign, y * sign)

  def *(value: Double): VectorBox2d = VectorBox2d(x * value, y * value)

  def *(interval: Interval): VectorBox2d = VectorBox2d(x * interval, y * interval)

  def /(value: Double): VectorBox2d = VectorBox2d(x / value, y / value)

  def /(interval: Interval): VectorBox2d = VectorBox2d(x / interval, y / interval)

  def dot(vector: Vector2d): Interval = x * vector.x + y * vector.y

  def dot(direction: Direction2d): Interval = x * direction.x + y * direction.y

  def dot(that: VectorBox2d): Interval = this.x * that.x + this.y * that.y

  def dot(directionBox: DirectionBox2d): Interval = x * directionBox.x + y * directionBox.y
}

object VectorBox2d {
  def fromComponents(components: Seq[Interval]): VectorBox2d = components match {
    case Seq(x, y) => VectorBox2d(x, y)
    case _ => throw new IllegalArgumentException("VectorBox2d requires 2 components")
  }

  val Zero: VectorBox2d = VectorBox2d(Interval.Zero, Interval.Zero)
}
