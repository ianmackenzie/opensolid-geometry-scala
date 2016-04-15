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

final case class Vector3d(x: Double, y: Double, z: Double) extends VectorTransformable3d[Vector3d] {
  def components: (Double, Double, Double) =
    (x, y, z)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Vector3d")
  }

  def squaredLength: Double =
    x * x + y * y + z * z

  def length: Double =
    math.sqrt(squaredLength)

  def isZero(tolerance: Double): Boolean =
    squaredLength.isZero(tolerance * tolerance)

  def isNonZero(tolerance: Double): Boolean =
    squaredLength.isNonZero(tolerance * tolerance)

  override def transformedBy(transformation: Transformation3d): Vector3d =
    transformation(this)

  def projectedOnto(direction: Direction3d): Vector3d =
    componentIn(direction) * direction

  def projectedOnto(axis: Axis3d): Vector3d =
    projectedOnto(axis.direction)

  def projectedOnto(plane: Plane3d): Vector3d =
    this - this.projectedOnto(plane.normalDirection)

  def projectedInto(plane: Plane3d): Vector2d =
    Vector2d(
      this.componentIn(plane.xDirection),
      this.componentIn(plane.yDirection)
    )

  def normalized: Vector3d = {
    val length = this.length
    if (length == 0.0) {
      throw GeometricException("Cannot normalize zero length vector")
    }
    this / length
  }

  def direction: Direction3d = {
    val length = this.length
    if (length == 0.0) {
      throw GeometricException("Cannot find direction of zero length vector")
    }
    Direction3d(x / length, y / length, z / length)
  }

  def perpendicularVector: Vector3d = {
    val absX = x.abs
    val absY = y.abs
    val absZ = z.abs
    if (absX <= absY) {
      if (absX <= absZ) {
        Vector3d(0, -z, y)
      } else {
        Vector3d(-y, x, 0)
      }
    } else {
      if (absY <= absZ) {
        Vector3d(z, 0, -x)
      } else {
        Vector3d(-y, x, 0)
      }
    }
  }

  def normalDirection: Direction3d =
    perpendicularVector.direction

  def unary_- : Vector3d =
    Vector3d(-x, -y, -z)

  def negated: Vector3d =
    -this

  def +(that: Vector3d): Vector3d =
    Vector3d(this.x + that.x, this.y + that.y, this.z + that.z)

  def +[P](expression: VectorExpression3d[P]): VectorExpression3d[P] =
    VectorExpression3d.Constant[P](this) + expression

  def plus(that: Vector3d): Vector3d =
    this + that

  def plus[P](expression: VectorExpression3d[P]): VectorExpression3d[P] =
    this + expression

  def -(that: Vector3d): Vector3d =
    Vector3d(this.x - that.x, this.y - that.y, this.z - that.z)

  def -[P](expression: VectorExpression3d[P]): VectorExpression3d[P] =
    VectorExpression3d.Constant[P](this) - expression

  def minus(that: Vector3d): Vector3d =
    this - that

  def minus[P](expression: VectorExpression3d[P]): VectorExpression3d[P] =
    this - expression

  def *(value: Double): Vector3d =
    Vector3d(x * value, y * value, z * value)

  def *[P](expression: Expression1d[P]): VectorExpression3d[P] =
    VectorExpression3d.Constant[P](this) * expression

  def times(value: Double): Vector3d =
    this * value

  def times[P](expression: Expression1d[P]): VectorExpression3d[P] =
    this * expression

  def /(value: Double): Vector3d =
    Vector3d(x / value, y / value, z / value)

  def /[P](expression: Expression1d[P]): VectorExpression3d[P] =
    VectorExpression3d.Constant[P](this) / expression

  def dividedBy(value: Double): Vector3d =
    this / value

  def dividedBy[P](expression: Expression1d[P]): VectorExpression3d[P] =
    this / expression

  def dot(that: Vector3d): Double =
    this.x * that.x + this.y * that.y + this.z * that.z

  def dot[P](expression: VectorExpression3d[P]): Expression1d[P] =
    VectorExpression3d.Constant[P](this).dot(expression)

  def cross(that: Vector3d): Vector3d =
    Vector3d(
      this.y * that.z - this.z * that.y,
      this.z * that.x - this.x * that.z,
      this.x * that.y - this.y * that.x
    )

  def cross(direction: Direction3d): Vector3d =
    Vector3d(
      y * direction.z - z * direction.y,
      z * direction.x - x * direction.z,
      x * direction.y - y * direction.x
    )

  def cross[P](expression: VectorExpression3d[P]): VectorExpression3d[P] =
    VectorExpression3d.Constant[P](this).cross(expression)

  def componentIn(direction: Direction3d): Double =
    x * direction.x + y * direction.y + z * direction.z
}

object Vector3d {
  def fromComponents(components: (Double, Double, Double)): Vector3d = components match {
    case (x, y, z) => Vector3d(x, y, z)
  }

  val Zero: Vector3d = Vector3d(0.0, 0.0, 0.0)
}
