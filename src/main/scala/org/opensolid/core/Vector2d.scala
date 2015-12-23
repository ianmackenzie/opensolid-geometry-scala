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
  def this(components: (Double, Double)) = this(components.first, components.second)

  def components: (Double, Double) = (x, y)

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

  def projectedOnto(axis: Axis2d): Vector2d = this.dot(axis.direction) * axis.direction

  def placedOnto(plane: Plane3d): Vector3d = x * plane.xDirection + y * plane.yDirection

  def normalized: Vector2d = this match {
    case Vector2d.Zero => this
    case _ => this / length
  }

  def direction: Direction2d = Direction2d(normalized)

  def normalDirection: Direction2d = direction.normalDirection

  def unary_- : Vector2d = Vector2d(-x, -y)

  def negated: Vector2d = -this

  def +(that: Vector2d): Vector2d = Vector2d(this.x + that.x, this.y + that.y)

  def plus(that: Vector2d): Vector2d = this + that

  def +(vectorBox: VectorBox2d): VectorBox2d = VectorBox2d(x + vectorBox.x, y + vectorBox.y)

  def plus(vectorBox: VectorBox2d): VectorBox2d = this + vectorBox

  def -(that: Vector2d): Vector2d = Vector2d(this.x - that.x, this.y - that.y)

  def minus(that: Vector2d): Vector2d = this - that

  def -(vectorBox: VectorBox2d): VectorBox2d = VectorBox2d(x - vectorBox.x, y - vectorBox.y)

  def minus(vectorBox: VectorBox2d): VectorBox2d = this - vectorBox

  def *(sign: Sign): Vector2d = sign match {
    case Sign.Positive => this
    case Sign.Negative => -this
    case _ => Vector2d.Zero
  }

  def times(sign: Sign): Vector2d = this * sign

  def *(value: Double): Vector2d = Vector2d(x * value, y * value)

  def times(value: Double): Vector2d = this * value

  def *(interval: Interval): VectorBox2d = VectorBox2d(x * interval, y * interval)

  def times(interval: Interval): VectorBox2d = this * interval

  def /(value: Double): Vector2d = Vector2d(x / value, y / value)

  def dividedBy(value: Double): Vector2d = this / value

  def /(interval: Interval): VectorBox2d = VectorBox2d(x / interval, y / interval)

  def dividedBy(interval: Interval): VectorBox2d = this / interval

  def dot(that: Vector2d): Double = this.x * that.x + this.y * that.y

  def dot(direction: Direction2d): Double = dot(direction.vector)

  def dot(vectorBox: VectorBox2d): Interval = x * vectorBox.x + y * vectorBox.y

  def dot(directionBox: DirectionBox2d): Interval = dot(directionBox.vectorBox)
}

object Vector2d {
  def apply(components: (Double, Double)): Vector2d = new Vector2d(components)

  def polar(radius: Double, angle: Double): Vector2d =
    Vector2d(radius * math.cos(angle), radius * math.sin(angle))

  val Zero: Vector2d = Vector2d(0.0, 0.0)
}
