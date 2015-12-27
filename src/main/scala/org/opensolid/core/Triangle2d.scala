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
  extends Transformable2d[Triangle2d] with Bounded[Box2d] {

  def this(vertices: (Point2d, Point2d, Point2d)) =
    this(vertices.first, vertices.second, vertices.third)

  def vertices: (Point2d, Point2d, Point2d) = (firstVertex, secondVertex, thirdVertex)

  override def transformedBy(transformation: Transformation2d): Triangle2d =
    Triangle2d(
      firstVertex.transformedBy(transformation),
      secondVertex.transformedBy(transformation),
      thirdVertex.transformedBy(transformation)
    )

  override def bounds: Box2d = firstVertex.hull(secondVertex).hull(thirdVertex)

  def area: Double = 0.5 * (secondVertex - firstVertex).cross(thirdVertex - firstVertex)
}
