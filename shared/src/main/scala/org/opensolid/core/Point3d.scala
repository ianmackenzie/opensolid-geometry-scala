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

final case class Point3d(x: Double, y: Double, z: Double)
  extends Scalable3d[Point3d] with Bounded[BoundingBox3d] {

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Point3d")
  }

  override def bounds: BoundingBox3d = BoundingBox3d(Interval(x), Interval(y), Interval(z))

  override def isEqualTo(other: Any, tolerance: Double): Boolean = other match {
    case that: Point3d => this.squaredDistanceTo(that).isZero(tolerance * tolerance)
    case _ => false
  }

  def squaredDistanceTo(that: Point3d): Double = (this - that).squaredLength

  def distanceTo(that: Point3d): Double = (this - that).length

  def isOrigin(tolerance: Double): Boolean = x * x + y * y + z * z <= tolerance * tolerance

  def distanceAlong(axis: Axis3d): Double = (this - axis.originPoint).dot(axis.direction)

  def squaredDistanceTo(axis: Axis3d): Double = (this - this.projectedOnto(axis)).squaredLength

  def distanceTo(axis: Axis3d): Double = math.sqrt(squaredDistanceTo(axis))

  def isOn(axis: Axis3d, tolerance: Double): Boolean =
    squaredDistanceTo(axis).isZero(tolerance * tolerance)

  def distanceTo(plane: Plane3d): Double = (this - plane.originPoint).dot(plane.normalDirection)

  def isOn(plane: Plane3d, tolerance: Double): Boolean = distanceTo(plane).isZero(tolerance)

  override def transformedBy(transformation: Transformation3d): Point3d = transformation(this)

  override def scaledAbout(point: Point3d, scale: Double): Point3d = point + scale * (this - point)

  def projectedOnto(axis: Axis3d): Point3d = axis.originPoint + distanceAlong(axis) * axis.direction

  def projectedOnto(plane: Plane3d): Point3d =
    this - (this - plane.originPoint).dot(plane.normalDirection) * plane.normalDirection

  def projectedInto(plane: Plane3d): Point2d = {
    val displacement = this - plane.originPoint
    Point2d(displacement.dot(plane.xDirection), displacement.dot(plane.yDirection))
  }

  def +(vector: Vector3d): Point3d = Point3d(x + vector.x, y + vector.y, z + vector.z)

  def +(vectorBoundingBox: VectorBoundingBox3d): BoundingBox3d =
    BoundingBox3d(x + vectorBoundingBox.x, y + vectorBoundingBox.y, z + vectorBoundingBox.z)

  def -(vector: Vector3d): Point3d = Point3d(x - vector.x, y - vector.y, z - vector.z)

  def -(vectorBoundingBox: VectorBoundingBox3d): BoundingBox3d =
    BoundingBox3d(x - vectorBoundingBox.x, y - vectorBoundingBox.y, z - vectorBoundingBox.z)

  def -(that: Point3d): Vector3d = Vector3d(x - that.x, y - that.y, z - that.z)

  def -(boundingBox: BoundingBox3d): VectorBoundingBox3d =
    VectorBoundingBox3d(x - boundingBox.x, y - boundingBox.y, z - boundingBox.z)
}

object Point3d {
  val Origin: Point3d = Point3d(0.0, 0.0, 0.0)

  def getOrigin: Point3d = Origin
}
