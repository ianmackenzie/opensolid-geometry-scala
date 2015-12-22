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

case class LineSegment3d(firstEndpoint: Point3d, secondEndpoint: Point3d)
  extends Scalable3d[LineSegment3d]
  with Bounded[BoundingBox3d]
  with GeometricallyComparable[LineSegment3d] {

  def this(endpoints: (Point3d, Point3d)) = this(endpoints.first, endpoints.second)

  def endpoints: (Point3d, Point3d) = (firstEndpoint, secondEndpoint)

  def vector: Vector3d = secondEndpoint - firstEndpoint

  def direction: Direction3d = vector.direction

  def normalDirection: Direction3d = direction.normalDirection

  def axis: Axis3d = Axis3d(firstEndpoint, direction)

  def length: Double = vector.length

  def squaredLength: Double = vector.squaredLength

  def midpoint: Point3d = firstEndpoint + 0.5 * vector

  override def bounds: BoundingBox3d = firstEndpoint.hull(secondEndpoint)

  override def isEqualTo(that: LineSegment3d, tolerance: Double): Boolean =
    this.firstEndpoint.isEqualTo(that.firstEndpoint, tolerance) &&
    this.secondEndpoint.isEqualTo(that.secondEndpoint, tolerance)

  override def scaledAbout(point: Point3d, scale: Double): LineSegment3d =
    LineSegment3d(
      firstEndpoint.scaledAbout(point, scale),
      secondEndpoint.scaledAbout(point, scale)
    )

  override def transformedBy(transformation: Transformation3d): LineSegment3d =
    LineSegment3d(
      firstEndpoint.transformedBy(transformation),
      secondEndpoint.transformedBy(transformation)
    )

  def projectedOnto(axis: Axis3d): LineSegment3d =
    LineSegment3d(firstEndpoint.projectedOnto(axis), secondEndpoint.projectedOnto(axis))

  def projectedOnto(plane: Plane3d): LineSegment3d =
    LineSegment3d(firstEndpoint.projectedOnto(plane), secondEndpoint.projectedOnto(plane))

  def projectedInto(plane: Plane3d): LineSegment2d =
    LineSegment2d(firstEndpoint.projectedInto(plane), secondEndpoint.projectedInto(plane))
}
