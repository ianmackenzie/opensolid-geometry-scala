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

case class Plane3d(
  originPoint: Point3d,
  xDirection: Direction3d,
  yDirection: Direction3d,
  normalDirection: Direction3d,
  handedness: Handedness
) extends Transformable3d[Plane3d] with Scalable3d[Plane3d] {

  require(handedness.sign == Sign.of(xDirection.cross(yDirection).dot(normalDirection)))

  override def transformedBy(transformation: Transformation3d): Plane3d =
    Plane3d(
      originPoint.transformedBy(transformation),
      xDirection.transformedBy(transformation),
      yDirection.transformedBy(transformation),
      normalDirection.transformedBy(transformation),
      handedness.transformedBy(transformation)
    )

  override def scaledAbout(point: Point3d, scale: Double): Plane3d = {
    require(scale > 0.0)
    Plane3d(
      originPoint.scaledAbout(point, scale),
      xDirection,
      yDirection,
      normalDirection,
      handedness
    )
  }
}

object Plane3d {
  def apply(originPoint: Point3d, normalDirection: Direction3d): Plane3d =
    Plane3d(originPoint, normalDirection, Handedness.Right)

  def apply(originPoint: Point3d, normalDirection: Direction3d, handedness: Handedness): Plane3d = {
    val xDirection = normalDirection.normalDirection
    val yDirection =
      handedness.sign * Direction3d.fromComponents(normalDirection.cross(xDirection).components)
    Plane3d(originPoint, xDirection, yDirection, normalDirection, handedness)
  }

  def apply(originPoint: Point3d, xDirection: Direction3d, yDirection: Direction3d): Plane3d =
    Plane3d(originPoint, xDirection, yDirection, Handedness.Right)

  def apply(
    originPoint: Point3d,
    xDirection: Direction3d,
    yDirection: Direction3d,
    handedness: Handedness
  ): Plane3d =
    Plane3d(
      originPoint,
      xDirection,
      yDirection,
      handedness.sign * Direction3d.fromComponents(xDirection.cross(yDirection).components),
      handedness
    )

  def throughPoints(firstPoint: Point3d, secondPoint: Point3d, thirdPoint: Point3d): Plane3d =
    Plane3d.throughPoints(firstPoint, secondPoint, thirdPoint, Handedness.Right)

  def throughPoints(
    firstPoint: Point3d,
    secondPoint: Point3d,
    thirdPoint: Point3d,
    handedness: Handedness
  ): Plane3d = {
    val firstLeg = secondPoint - firstPoint;
    val secondLeg = thirdPoint - firstPoint;
    val xDirection = firstLeg.direction;
    val rightHandedNormalDirection = firstLeg.cross(secondLeg).direction;
    val yDirection =
      Direction3d.fromComponents(rightHandedNormalDirection.cross(xDirection).components);
    Plane3d(
      firstPoint,
      xDirection,
      yDirection,
      handedness.sign * rightHandedNormalDirection,
      handedness
    );
  }
}
