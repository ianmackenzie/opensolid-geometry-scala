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

final case class DirectionBox2d(x: Interval, y: Interval) {
  def components: Array[Interval] = Array(x, y)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case _ =>
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds for DirectionBox2d")
  }

  def vector: VectorBox2d = VectorBox2d(x, y)

  def unary_- : DirectionBox2d = DirectionBox2d(-x, -y)

  def *(sign: Sign): DirectionBox2d = DirectionBox2d(x * sign, y * sign)

  def *(value: Double): VectorBox2d = VectorBox2d(x * value, y * value)

  def *(interval: Interval): VectorBox2d = VectorBox2d(x * interval, y * interval)

  def /(value: Double): VectorBox2d = VectorBox2d(x / value, y / value)

  def /(interval: Interval): VectorBox2d = VectorBox2d(x / interval, y / interval)

  def dot(vector: Vector2d): Interval = x * vector.x + y * vector.y

  def dot(vectorBox: VectorBox2d): Interval = x * vectorBox.x + y * vectorBox.y

  def dot(direction: Direction2d): Interval = x * direction.x + y * direction.y

  def dot(that: DirectionBox2d): Interval = this.x * that.x + this.y * that.y
}

object DirectionBox2d {
  def apply(direction: Direction2d): DirectionBox2d =
    DirectionBox2d(Interval(direction.x), Interval(direction.y))

  def fromComponents(components: Seq[Interval]): DirectionBox2d = components match {
    case Seq(x, y) => DirectionBox2d(x, y)
    case _ => throw new IllegalArgumentException("DirectionBox2d requires 2 components")
  }

  val None = DirectionBox2d(Interval.Empty, Interval.Empty)
}
