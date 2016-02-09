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

final case class LineSegment2d(startPoint: Point2d, endPoint: Point2d)
  extends Scalable2d[LineSegment2d]
  with Bounded[Bounds2d]
  with GeometricallyComparable[LineSegment2d]
  with Curve2d {

  def endpoints: (Point2d, Point2d) =
    (startPoint, endPoint)

  def vector: Vector2d =
    endPoint - startPoint

  def direction: Direction2d =
    vector.direction

  def normalDirection: Direction2d =
    direction.normalDirection

  def axis: Axis2d =
    Axis2d(startPoint, direction)

  def reversed: LineSegment2d =
    LineSegment2d(endPoint, startPoint)

  def length: Double =
    vector.length

  def squaredLength: Double =
    vector.squaredLength

  def midpoint: Point2d =
    Point2d.midpoint(startPoint, endPoint)

  override def bounds: Bounds2d =
    startPoint.hull(endPoint)

  override def equals(that: LineSegment2d, tolerance: Double): Boolean =
    this.startPoint.equals(that.startPoint, tolerance) &&
    this.endPoint.equals(that.endPoint, tolerance)

  override def scaledAbout(point: Point2d, scale: Double): LineSegment2d = {
    require(scale > 0.0)
    LineSegment2d(startPoint.scaledAbout(point, scale), endPoint.scaledAbout(point, scale))
  }

  override def transformedBy(transformation: Transformation2d): LineSegment2d =
    LineSegment2d(
      startPoint.transformedBy(transformation),
      endPoint.transformedBy(transformation)
    )

  def projectedOnto(axis: Axis2d): LineSegment2d =
    LineSegment2d(startPoint.projectedOnto(axis), endPoint.projectedOnto(axis))

  def placedOnto(plane: Plane3d): LineSegment3d =
    LineSegment3d(startPoint.placedOnto(plane), endPoint.placedOnto(plane))

  def parameterized: ParametricCurve2d =
    parameterized(startPoint + CurveParameter * vector, Interval.Unit)
}

object LineSegment2d {
  def fromEndpoints(endpoints: (Point2d, Point2d)): LineSegment2d = endpoints match {
    case (startPoint, endPoint) => LineSegment2d(startPoint, endPoint)
  }
}
