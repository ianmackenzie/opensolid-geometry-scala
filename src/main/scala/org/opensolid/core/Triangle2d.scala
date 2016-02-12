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

final case class Triangle2d(firstVertex: Point2d, secondVertex: Point2d, thirdVertex: Point2d)
  extends Scalable2d[Triangle2d] with Bounded[Bounds2d] with GeometricallyComparable[Triangle2d] {

  def vertices: (Point2d, Point2d, Point2d) = (firstVertex, secondVertex, thirdVertex)

  def vertex(index: Int): Point2d = index match {
    case 0 => firstVertex
    case 1 => secondVertex
    case 2 => thirdVertex
    case _ =>
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds for a triangle vertex")
  }

  def edges: (LineSegment2d, LineSegment2d, LineSegment2d) =
    (
      LineSegment2d(thirdVertex, secondVertex),
      LineSegment2d(firstVertex, thirdVertex),
      LineSegment2d(secondVertex, firstVertex)
    )

  def edge(index: Int): LineSegment2d = index match {
    case 0 => LineSegment2d(thirdVertex, secondVertex)
    case 1 => LineSegment2d(firstVertex, thirdVertex)
    case 2 => LineSegment2d(secondVertex, firstVertex)
    case _ =>
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds for a triangle edge")
  }

  override def transformedBy(transformation: Transformation2d): Triangle2d =
    Triangle2d(
      firstVertex.transformedBy(transformation),
      secondVertex.transformedBy(transformation),
      thirdVertex.transformedBy(transformation)
    )

  override def scaledAbout(point: Point2d, scale: Double): Triangle2d =
    Triangle2d(
      firstVertex.scaledAbout(point, scale),
      secondVertex.scaledAbout(point, scale),
      thirdVertex.scaledAbout(point, scale)
    )

  override def bounds: Bounds2d = firstVertex.hull(secondVertex).hull(thirdVertex)

  override def equals(that: Triangle2d, tolerance: Double): Boolean =
    this.firstVertex.equals(that.firstVertex, tolerance) &&
    this.secondVertex.equals(that.secondVertex, tolerance) &&
    this.thirdVertex.equals(that.thirdVertex, tolerance)

  def area: Double = 0.5 * (secondVertex - firstVertex).cross(thirdVertex - firstVertex)

  def centroid: Point2d =
    firstVertex + ((secondVertex - firstVertex) + (thirdVertex - firstVertex)) / 3.0

  def contains(point: Point2d): Boolean = {
    val firstProduct = (secondVertex - firstVertex).cross(point - firstVertex)
    val secondProduct = (thirdVertex - secondVertex).cross(point - secondVertex)
    val thirdProduct = (firstVertex - thirdVertex).cross(point - thirdVertex)

    (firstProduct >= 0 && secondProduct >= 0 && thirdProduct >= 0) ||
    (firstProduct <= 0 && secondProduct <= 0 && thirdProduct <= 0)
  }

  def placedOnto(plane: Plane3d): Triangle3d =
    Triangle3d(
      firstVertex.placedOnto(plane),
      secondVertex.placedOnto(plane),
      thirdVertex.placedOnto(plane)
    )
}

object Triangle2d {
  val Unit: Triangle2d = Triangle2d(Point2d.Origin, Point2d(1, 0), Point2d(0, 1))
}
