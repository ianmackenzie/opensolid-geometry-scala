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

final case class Triangle3d(firstVertex: Point3d, secondVertex: Point3d, thirdVertex: Point3d)
  extends Transformable3d[Triangle3d] with Bounded[Box3d] with GeometricallyComparable[Triangle3d] {

  def this(vertices: (Point3d, Point3d, Point3d)) =
    this(vertices.first, vertices.second, vertices.third)

  def vertices: (Point3d, Point3d, Point3d) = (firstVertex, secondVertex, thirdVertex)

  def vertex(index: Int): Point3d = index match {
    case 0 => firstVertex
    case 1 => secondVertex
    case 2 => thirdVertex
    case _ =>
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds for a triangle vertex")
  }

  def edges: (LineSegment3d, LineSegment3d, LineSegment3d) =
    (
      LineSegment3d(thirdVertex, secondVertex),
      LineSegment3d(firstVertex, thirdVertex),
      LineSegment3d(secondVertex, firstVertex)
    )

  def edge(index: Int): LineSegment3d = index match {
    case 0 => LineSegment3d(thirdVertex, secondVertex)
    case 1 => LineSegment3d(firstVertex, thirdVertex)
    case 2 => LineSegment3d(secondVertex, firstVertex)
    case _ =>
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds for a triangle edge")
  }

  override def transformedBy(transformation: Transformation3d): Triangle3d =
    Triangle3d(
      firstVertex.transformedBy(transformation),
      secondVertex.transformedBy(transformation),
      thirdVertex.transformedBy(transformation)
    )

  override def bounds: Box3d = firstVertex.hull(secondVertex).hull(thirdVertex)

  override def isEqualTo(that: Triangle3d, tolerance: Double): Boolean =
    this.firstVertex.isEqualTo(that.firstVertex, tolerance) &&
    this.secondVertex.isEqualTo(that.secondVertex, tolerance) &&
    this.thirdVertex.isEqualTo(that.thirdVertex, tolerance)

  def area: Double = 0.5 * (secondVertex - firstVertex).cross(thirdVertex - firstVertex).length

  def normalDirection: Direction3d =
    numerics.normalDirectionFromThreePoints(firstVertex, secondVertex, thirdVertex)

  def centroid: Point3d =
    firstVertex + ((secondVertex - firstVertex) + (thirdVertex - firstVertex)) / 3.0

  def plane: Plane3d = Plane3d(firstVertex, normalDirection)

  def projectedOnto(plane: Plane3d): Triangle3d =
    Triangle3d(
      firstVertex.projectedOnto(plane),
      secondVertex.projectedOnto(plane),
      thirdVertex.projectedOnto(plane)
    )

  def projectedInto(plane: Plane3d): Triangle2d =
    Triangle2d(
      firstVertex.projectedInto(plane),
      secondVertex.projectedInto(plane),
      thirdVertex.projectedInto(plane)
    )
}

object Triangle3d {
  def apply(vertices: (Point3d, Point3d, Point3d)): Triangle3d = new Triangle3d(vertices)
}
