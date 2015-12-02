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
  def components: Array[Double] = Array(x, y, z)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Vector3d")
  }

  def squaredLength: Double = x * x + y * y + z * z

  def length: Double = math.sqrt(squaredLength)

  def isZero(tolerance: Double): Boolean = squaredLength.isZero(tolerance * tolerance)

  def isNotZero(tolerance: Double): Boolean = squaredLength.isNotZero(tolerance * tolerance)

  override def transformedBy(transformation: Transformation3d): Vector3d = transformation(this)

  def projectedOnto(axis: Axis3d): Vector3d = this.dot(axis.direction) * axis.direction

  def projectedOnto(plane: Plane3d): Vector3d =
    this - this.dot(plane.normalDirection) * plane.normalDirection

  def projectedInto(plane: Plane3d): Vector2d =
    Vector2d(this.dot(plane.xDirection), this.dot(plane.yDirection))

  def normalized: Vector3d = direction.vector

  def direction: Direction3d = {
    if (this == Vector3d.Zero) {
      Direction3d.None
    } else {
      val length = this.length
      Direction3d(x / length, y / length, z / length)
    }
  }

  def unary_- : Vector3d = Vector3d(-x, -y, -z)

  def +(that: Vector3d): Vector3d = Vector3d(this.x + that.x, this.y + that.y, this.z + that.z)

  def +(vectorBoundingBox: VectorBoundingBox3d): VectorBoundingBox3d =
    VectorBoundingBox3d(x + vectorBoundingBox.x, y + vectorBoundingBox.y, z + vectorBoundingBox.z)

  def -(that: Vector3d): Vector3d = Vector3d(this.x - that.x, this.y - that.y, this.z - that.z)

  def -(vectorBoundingBox: VectorBoundingBox3d): VectorBoundingBox3d =
    VectorBoundingBox3d(x - vectorBoundingBox.x, y - vectorBoundingBox.y, z - vectorBoundingBox.z)

  def *(sign: Sign): Vector3d = sign match {
    case Sign.Positive => this
    case Sign.Negative => -this
    case _ => Vector3d.Zero
  }

  def *(value: Double): Vector3d = Vector3d(x * value, y * value, z * value)

  def *(interval: Interval): VectorBoundingBox3d =
    VectorBoundingBox3d(x * interval, y * interval, z * interval)

  def /(value: Double): Vector3d = Vector3d(x / value, y / value, z / value)

  def /(interval: Interval): VectorBoundingBox3d =
    VectorBoundingBox3d(x / interval, y / interval, z / interval)

  def dot(that: Vector3d): Double = this.x * that.x + this.y * that.y + this.z * that.z

  def dot(direction: Direction3d): Double = dot(direction.vector)

  def dot(vectorBoundingBox: VectorBoundingBox3d): Interval =
    x * vectorBoundingBox.x + y * vectorBoundingBox.y + z * vectorBoundingBox.z

  def dot(directionBoundingBox: DirectionBoundingBox3d): Interval =
    dot(directionBoundingBox.vectorBoundingBox)

  def cross(that: Vector3d): Vector3d =
    Vector3d(
      this.y * that.z - this.z * that.y,
      this.z * that.x - this.x * that.z,
      this.x * that.y - this.y * that.x
    )

  def cross(direction: Direction3d): Vector3d = cross(direction.vector)

  def cross(vectorBoundingBox: VectorBoundingBox3d): VectorBoundingBox3d =
    VectorBoundingBox3d(
      y * vectorBoundingBox.z - z * vectorBoundingBox.y,
      z * vectorBoundingBox.x - x * vectorBoundingBox.z,
      x * vectorBoundingBox.y - y * vectorBoundingBox.x
    )

  def cross(directionBoundingBox: DirectionBoundingBox3d): VectorBoundingBox3d =
    cross(directionBoundingBox.vectorBoundingBox)

  def normalDirection: Direction3d = {
    if (this == Vector3d.Zero) {
      Direction3d.None
    } else {
      val absX = x.abs
      val absY = y.abs
      val absZ = z.abs
      if (absX <= absY) {
        if (absX <= absZ) {
          Direction3d.X.cross(this).direction
        } else {
          Direction3d.Z.cross(this).direction
        }
      } else {
        if (absY <= absZ) {
          Direction3d.Y.cross(this).direction
        } else {
          Direction3d.Z.cross(this).direction
        }
      }
    }
  }
}

object Vector3d {
  def fromComponents[T <% Double](components: Seq[T]): Vector3d = components match {
    case Seq(x, y, z) => Vector3d(x, y, z)
    case _ => throw new IllegalArgumentException("Vector3d requires 3 components")
  }

  val Zero: Vector3d = Vector3d(0.0, 0.0, 0.0)
}
