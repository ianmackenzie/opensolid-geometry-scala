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
  extends Scalable3d[Point3d] with Bounded[Bounds3d] with GeometricallyComparable[Point3d] {

  def components: (Double, Double, Double) = (x, y, z)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Point3d")
  }

  override def bounds: Bounds3d =
    Bounds3d(Interval.singleton(x), Interval.singleton(y), Interval.singleton(z))

  override def equals(that: Point3d, tolerance: Double): Boolean =
    this.squaredDistanceTo(that).isZero(tolerance * tolerance)

  def squaredDistanceTo(that: Point3d): Double = (this - that).squaredLength

  def distanceTo(that: Point3d): Double = (this - that).length

  def isOrigin(tolerance: Double): Boolean = x * x + y * y + z * z <= tolerance * tolerance

  def distanceAlong(axis: Axis3d): Double = (this - axis.originPoint).componentIn(axis.direction)

  def squaredDistanceTo(axis: Axis3d): Double =
    (this - axis.originPoint).cross(axis.direction.vector).squaredLength

  def distanceTo(axis: Axis3d): Double = math.sqrt(squaredDistanceTo(axis))

  def isOn(axis: Axis3d, tolerance: Double): Boolean =
    squaredDistanceTo(axis).isZero(tolerance * tolerance)

  def distanceTo(plane: Plane3d): Double =
    (this - plane.originPoint).componentIn(plane.normalDirection)

  def isOn(plane: Plane3d, tolerance: Double): Boolean = distanceTo(plane).isZero(tolerance)

  override def transformedBy(transformation: Transformation3d): Point3d = transformation(this)

  override def scaledAbout(point: Point3d, scale: Double): Point3d = point + scale * (this - point)

  def projectedOnto(axis: Axis3d): Point3d =
    axis.originPoint + (this - axis.originPoint).projectedOnto(axis)

  def projectedOnto(plane: Plane3d): Point3d =
    this - (this - plane.originPoint).projectedOnto(plane.normalDirection)

  def projectedInto(plane: Plane3d): Point2d = {
    val displacement = this - plane.originPoint
    Point2d(
      displacement.componentIn(plane.xDirection),
      displacement.componentIn(plane.yDirection)
    )
  }

  def hull(that: Point3d): Bounds3d =
    Bounds3d(this.x.hull(that.x), this.y.hull(that.y), this.z.hull(that.z))

  def hull(bounds: Bounds3d): Bounds3d =
    Bounds3d(x.hull(bounds.x), y.hull(bounds.y), z.hull(bounds.z))

  def +(vector: Vector3d): Point3d = Point3d(x + vector.x, y + vector.y, z + vector.z)

  def +(vectorBounds: VectorBounds3d): Bounds3d =
    Bounds3d(x + vectorBounds.x, y + vectorBounds.y, z + vectorBounds.z)

  def -(vector: Vector3d): Point3d = Point3d(x - vector.x, y - vector.y, z - vector.z)

  def -(vectorBounds: VectorBounds3d): Bounds3d =
    Bounds3d(x - vectorBounds.x, y - vectorBounds.y, z - vectorBounds.z)

  def -(that: Point3d): Vector3d = Vector3d(x - that.x, y - that.y, z - that.z)

  def -(bounds: Bounds3d): VectorBounds3d =
    VectorBounds3d(x - bounds.x, y - bounds.y, z - bounds.z)
}

object Point3d {
  val Origin: Point3d = Point3d(0.0, 0.0, 0.0)
}
