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
  def components: (Double, Double) =
    (x, y)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Vector2d")
  }

  def squaredLength: Double =
    x * x + y * y

  def length: Double =
    math.sqrt(squaredLength)

  def isZero(tolerance: Double): Boolean =
    squaredLength.isZero(tolerance * tolerance)

  def isNotZero(tolerance: Double): Boolean =
    squaredLength.isNotZero(tolerance * tolerance)

  override def transformedBy(transformation: Transformation2d): Vector2d =
    transformation(this)

  def projectedOnto(direction: Direction2d): Vector2d =
    componentIn(direction) * direction

  def projectedOnto(axis: Axis2d): Vector2d =
    projectedOnto(axis.direction)

  def placedOnto(plane: Plane3d): Vector3d =
    x * plane.xDirection + y * plane.yDirection

  def normalized: Vector2d = {
    val length = this.length
    if (length == 0.0) {
      throw GeometricException("Cannot normalize zero length vector")
    }
    this / length
  }

  def direction: Direction2d = {
    val length = this.length
    if (length == 0.0) {
      throw GeometricException("Cannot find direction of zero length vector")
    }
    Direction2d(x / length, y / length)
  }

  def perpendicularVector: Vector2d =
    Vector2d(-y, x)

  def normalDirection: Direction2d =
    perpendicularVector.direction

  def unary_- : Vector2d =
    Vector2d(-x, -y)

  def negated: Vector2d =
    -this

  def +(that: Vector2d): Vector2d =
    Vector2d(this.x + that.x, this.y + that.y)

  def +[P](expression: VectorExpression2d[P]): VectorExpression2d[P] =
    VectorExpression2d.Constant[P](this) + expression

  def plus(that: Vector2d): Vector2d =
    this + that

  def plus[P](expression: VectorExpression2d[P]): VectorExpression2d[P] =
    this + expression

  def -(that: Vector2d): Vector2d =
    Vector2d(this.x - that.x, this.y - that.y)

  def -[P](expression: VectorExpression2d[P]): VectorExpression2d[P] =
    VectorExpression2d.Constant[P](this) - expression

  def minus(that: Vector2d): Vector2d =
    this - that

  def minus[P](expression: VectorExpression2d[P]): VectorExpression2d[P] =
    this - expression

  def *(value: Double): Vector2d =
    Vector2d(x * value, y * value)

  def *[P](expression: ScalarExpression[P]): VectorExpression2d[P] =
    VectorExpression2d.Constant[P](this) * expression

  def times(value: Double): Vector2d =
    this * value

  def times[P](expression: ScalarExpression[P]): VectorExpression2d[P] =
    this * expression

  def /(value: Double): Vector2d =
    Vector2d(x / value, y / value)

  def /[P](expression: ScalarExpression[P]): VectorExpression2d[P] =
    VectorExpression2d.Constant[P](this) / expression

  def dividedBy(value: Double): Vector2d =
    this / value

  def dividedBy[P](expression: ScalarExpression[P]): VectorExpression2d[P] =
    this / expression

  def dot(that: Vector2d): Double =
    this.x * that.x + this.y * that.y

  def dot[P](expression: VectorExpression2d[P]): ScalarExpression[P] =
    VectorExpression2d.Constant[P](this).dot(expression)

  def cross(that: Vector2d): Double =
    this.x * that.y - this.y * that.x

  def cross[P](expression: VectorExpression2d[P]): ScalarExpression[P] =
    VectorExpression2d.Constant[P](this).cross(expression)

  def componentIn(direction: Direction2d): Double =
    x * direction.x + y * direction.y
}

object Vector2d {
  def fromComponents(components: (Double, Double)): Vector2d = components match {
    case (x, y) => Vector2d(x, y)
  }

  def polar(radius: Double, angle: Double): Vector2d =
    Vector2d(radius * math.cos(angle), radius * math.sin(angle))

  val Zero: Vector2d = Vector2d(0.0, 0.0)
}
