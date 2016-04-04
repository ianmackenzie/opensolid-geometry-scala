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

final case class Point3d(x: Double, y: Double, z: Double) extends Scalable3d[Point3d]
  with Bounded[Bounds3d]
  with GeometricallyComparable[Point3d] {

  def components: (Double, Double, Double) =
    (x, y, z)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Point3d")
  }

  override def bounds: Bounds3d =
    Bounds3d.singleton(this)

  override def isEqualTo(that: Point3d, tolerance: Double): Boolean =
    squaredDistanceTo(that).isZero(tolerance * tolerance)

  def squaredDistanceTo(that: Point3d): Double =
    vectorTo(that).squaredLength

  def squaredDistanceTo[P](expression: PointExpression3d[P]): ScalarExpression[P] =
    PointExpression3d.Constant[P](this).squaredDistanceTo(expression)

  def distanceTo(that: Point3d): Double =
    vectorTo(that).length

  def distanceTo[P](expression: PointExpression3d[P]): ScalarExpression[P] =
    PointExpression3d.Constant[P](this).distanceTo(expression)

  def isOrigin(tolerance: Double): Boolean =
    x * x + y * y + z * z <= tolerance * tolerance

  def distanceAlong(axis: Axis3d): Double =
    axis.originPoint.vectorTo(this).componentIn(axis.direction)

  def squaredDistanceTo(axis: Axis3d): Double =
    vectorTo(axis.originPoint).cross(axis.direction).squaredLength

  def distanceTo(axis: Axis3d): Double =
    math.sqrt(squaredDistanceTo(axis))

  def isOn(axis: Axis3d, tolerance: Double): Boolean =
    squaredDistanceTo(axis).isZero(tolerance * tolerance)

  def distanceTo(plane: Plane3d): Double =
    plane.signedDistanceTo(this).abs

  def isOn(plane: Plane3d, tolerance: Double): Boolean =
    plane.signedDistanceTo(this).isZero(tolerance)

  override def transformedBy(transformation: Transformation3d): Point3d =
    transformation(this)

  override def scaledAbout(point: Point3d, scale: Double): Point3d =
    point + scale * point.vectorTo(this)

  def projectedOnto(axis: Axis3d): Point3d =
    axis.originPoint + axis.originPoint.vectorTo(this).projectedOnto(axis)

  def projectedOnto(plane: Plane3d): Point3d =
    this + vectorTo(plane.originPoint).projectedOnto(plane.normalDirection)

  def projectedInto(plane: Plane3d): Point2d = {
    val displacement = plane.originPoint.vectorTo(this)
    Point2d(displacement.componentIn(plane.xDirection), displacement.componentIn(plane.yDirection))
  }

  def hull(that: Point3d): Bounds3d =
    Bounds3d(this.x.hull(that.x), this.y.hull(that.y), this.z.hull(that.z))

  def hull(bounds: Bounds3d): Bounds3d =
    Bounds3d(x.hull(bounds.x), y.hull(bounds.y), z.hull(bounds.z))

  def +(vector: Vector3d): Point3d =
    Point3d(x + vector.x, y + vector.y, z + vector.z)

  def +[P](vectorExpression: VectorExpression3d[P]): PointExpression3d[P] =
    PointExpression3d.Constant[P](this) + vectorExpression

  def plus(vector: Vector3d): Point3d =
    this + vector

  def plus[P](vectorExpression: VectorExpression3d[P]): PointExpression3d[P] =
    this + vectorExpression

  def -(vector: Vector3d): Point3d =
    Point3d(x - vector.x, y - vector.y, z - vector.z)

  def -[P](vectorExpression: VectorExpression3d[P]): PointExpression3d[P] =
    PointExpression3d.Constant[P](this) - vectorExpression

  def minus(vector: Vector3d): Point3d =
    this - vector

  def minus[P](vectorExpression: VectorExpression3d[P]): PointExpression3d[P] =
    this - vectorExpression

  def vectorTo(that: Point3d): Vector3d =
    Vector3d(that.x - this.x, that.y - this.y, that.z - this.z)

  def vectorTo[P](pointExpression: PointExpression3d[P]): VectorExpression3d[P] =
    PointExpression3d.Constant[P](this).vectorTo(pointExpression)
}

object Point3d {
  def midpoint(firstPoint: Point3d, secondPoint: Point3d): Point3d =
    firstPoint + 0.5 * firstPoint.vectorTo(secondPoint)

  val Origin: Point3d = Point3d(0.0, 0.0, 0.0)
}
