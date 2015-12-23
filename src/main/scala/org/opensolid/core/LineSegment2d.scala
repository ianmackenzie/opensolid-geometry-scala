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

case class LineSegment2d(firstEndpoint: Point2d, secondEndpoint: Point2d)
  extends Scalable2d[LineSegment2d]
  with Bounded[Box2d]
  with GeometricallyComparable[LineSegment2d] {

  def this(endpoints: (Point2d, Point2d)) = this(endpoints.first, endpoints.second)

  def endpoints: (Point2d, Point2d) = (firstEndpoint, secondEndpoint)

  def vector: Vector2d = secondEndpoint - firstEndpoint

  def direction: Direction2d = vector.direction

  def normalDirection: Direction2d = direction.normalDirection

  def normalDirection(handedness: Handedness): Direction2d = direction.normalDirection(handedness)

  def axis: Axis2d = Axis2d(firstEndpoint, direction)

  def length: Double = vector.length

  def squaredLength: Double = vector.squaredLength

  def midpoint: Point2d = firstEndpoint + 0.5 * vector

  override def bounds: Box2d = firstEndpoint.hull(secondEndpoint)

  override def isEqualTo(that: LineSegment2d, tolerance: Double): Boolean =
    this.firstEndpoint.isEqualTo(that.firstEndpoint, tolerance) &&
    this.secondEndpoint.isEqualTo(that.secondEndpoint, tolerance)

  override def scaledAbout(point: Point2d, scale: Double): LineSegment2d = {
    require(scale > 0.0)
    LineSegment2d(firstEndpoint.scaledAbout(point, scale), secondEndpoint.scaledAbout(point, scale))
  }

  override def transformedBy(transformation: Transformation2d): LineSegment2d =
    LineSegment2d(
      firstEndpoint.transformedBy(transformation),
      secondEndpoint.transformedBy(transformation)
    )

  def projectedOnto(axis: Axis2d): LineSegment2d =
    LineSegment2d(firstEndpoint.projectedOnto(axis), secondEndpoint.projectedOnto(axis))

  def placedOnto(plane: Plane3d): LineSegment3d =
    LineSegment3d(firstEndpoint.placedOnto(plane), secondEndpoint.placedOnto(plane))
}

object LineSegment2d {
  def apply(endpoints: (Point2d, Point2d)): LineSegment2d = new LineSegment2d(endpoints)
}
