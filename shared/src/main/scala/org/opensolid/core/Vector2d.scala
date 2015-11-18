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

import scala.math

final case class Vector2d(x: Double, y: Double) extends VectorTransformable2d[Vector2d] {
  def components: Array[Double] = Array(x, y)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Vector2d")
  }

  def squaredLength: Double = x * x + y * y

  def length: Double = math.sqrt(squaredLength)

  def isZero(tolerance: Double): Boolean = squaredLength.isZero(tolerance * tolerance)

  def isNotZero(tolerance: Double): Boolean = squaredLength.isNotZero(tolerance * tolerance)

  override def transformedBy(transformation: Transformation2d): Vector2d = transformation(this)

  def normalized: Vector2d = direction.vector

  def direction: Direction2d = {
    if (this == Vector2d.Zero) {
      Direction2d.None
    } else {
      val length = this.length
      Direction2d(x / length, y / length)
    }
  }

  def orthogonalDirection: Direction2d = direction.orthogonalDirection

  def unary_- : Vector2d = Vector2d(-x, -y)

  def +(that: Vector2d): Vector2d = Vector2d(this.x + that.x, this.y + that.y)

  def +(vectorBox: VectorBox2d): VectorBox2d = VectorBox2d(x + vectorBox.x, y + vectorBox.y)

  def -(that: Vector2d): Vector2d = Vector2d(this.x - that.x, this.y - that.y)

  def -(vectorBox: VectorBox2d): VectorBox2d = VectorBox2d(x - vectorBox.x, y - vectorBox.y)

  def *(sign: Sign): Vector2d = Vector2d(x * sign, y * sign)

  def *(value: Double): Vector2d = Vector2d(x * value, y * value)

  def *(interval: Interval): VectorBox2d = VectorBox2d(x * interval, y * interval)

  def /(value: Double): Vector2d = Vector2d(x / value, y / value)

  def /(interval: Interval): VectorBox2d = VectorBox2d(x / interval, y / interval)

  def dot(that: Vector2d): Double = this.x * that.x + this.y * that.y

  def dot(direction: Direction2d): Double = x * direction.x + y * direction.y

  def dot(vectorBox: VectorBox2d): Interval = x * vectorBox.x + y * vectorBox.y

  def dot(directionBox: DirectionBox2d): Interval = x * directionBox.x + y * directionBox.y
}

object Vector2d {
  def fromComponents[T <% Double](components: Seq[T]): Vector2d = components match {
    case Seq(x, y) => Vector2d(x, y)
    case _ => throw new IllegalArgumentException("Vector2d requires 2 components")
  }

  def polar(radius: Double, angle: Double): Vector2d =
    Vector2d(radius * math.cos(angle), radius * math.sin(angle))

  val Zero: Vector2d = Vector2d(0.0, 0.0)
}
