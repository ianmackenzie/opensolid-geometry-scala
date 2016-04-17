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

final case class Point2d(x: Double, y: Double) extends Scalable2d[Point2d]
  with Bounded[Bounds2d]
  with GeometricallyComparable[Point2d] {

  def components: (Double, Double) =
    (x, y)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Point2d")
  }

  override def bounds: Bounds2d =
    Bounds2d.singleton(this)

  override def isEqualTo(that: Point2d, tolerance: Double): Boolean =
    squaredDistanceTo(that).isZero(tolerance * tolerance)

  def squaredDistanceTo(that: Point2d): Double =
    vectorTo(that).squaredLength

  def squaredDistanceTo[P](expression: Expression2d[P]): Expression1d[P] =
    Expression2d.Constant[P](this).squaredDistanceTo(expression)

  def distanceTo(that: Point2d): Double =
    vectorTo(that).length

  def distanceTo[P](expression: Expression2d[P]): Expression1d[P] =
    Expression2d.Constant[P](this).distanceTo(expression)

  def isOrigin(tolerance: Double): Boolean =
    x * x + y * y <= tolerance * tolerance

  def distanceAlong(axis: Axis2d): Double =
    axis.originPoint.vectorTo(this).componentIn(axis.direction)

  def distanceTo(axis: Axis2d): Double =
    axis.signedDistanceTo(this).abs

  def isOn(axis: Axis2d, tolerance: Double): Boolean =
    axis.signedDistanceTo(this).isZero(tolerance)

  override def transformedBy(transformation: Transformation2d): Point2d =
    transformation(this)

  override def scaledAbout(point: Point2d, scale: Double): Point2d =
    point + scale * point.vectorTo(this)

  def projectedOnto(axis: Axis2d): Point2d =
    axis.originPoint + axis.originPoint.vectorTo(this).projectedOnto(axis)

  def placedOnto(plane: Plane3d): Point3d =
    plane.originPoint + x * plane.xDirection + y * plane.yDirection

  def hull(that: Point2d): Bounds2d =
    Bounds2d(this.x.hull(that.x), this.y.hull(that.y))

  def hull(bounds: Bounds2d): Bounds2d =
    Bounds2d(x.hull(bounds.x), y.hull(bounds.y))

  def +(vector: Vector2d): Point2d =
    Point2d(x + vector.x, y + vector.y)

  def +[P](vectorExpression: VectorExpression2d[P]): Expression2d[P] =
    Expression2d.Constant[P](this) + vectorExpression

  def plus(vector: Vector2d): Point2d =
    this + vector

  def plus[P](vectorExpression: VectorExpression2d[P]): Expression2d[P] =
    this + vectorExpression

  def -(vector: Vector2d): Point2d =
    Point2d(x - vector.x, y - vector.y)

  def -[P](vectorExpression: VectorExpression2d[P]): Expression2d[P] =
    Expression2d.Constant[P](this) - vectorExpression

  def minus(vector: Vector2d): Point2d =
    this - vector

  def minus[P](vectorExpression: VectorExpression2d[P]): Expression2d[P] =
    this - vectorExpression

  def vectorTo(that: Point2d): Vector2d =
    Vector2d(that.x - this.x, that.y - this.y)

  def vectorTo[P](pointExpression: Expression2d[P]): VectorExpression2d[P] =
    Expression2d.Constant[P](this).vectorTo(pointExpression)
}

object Point2d {
  def polar(radius: Double, angle: Double): Point2d =
    Point2d(radius * math.cos(angle), radius * math.sin(angle))

  def midpoint(firstPoint: Point2d, secondPoint: Point2d): Point2d =
    firstPoint + 0.5 * firstPoint.vectorTo(secondPoint)

  val Origin: Point2d = Point2d(0.0, 0.0)
}
