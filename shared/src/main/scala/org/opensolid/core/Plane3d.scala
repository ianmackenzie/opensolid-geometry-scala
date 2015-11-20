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
  normalDirection: Direction3d,
  handedness: Handedness,
  xDirection: Direction3d,
  yDirection: Direction3d
) extends Transformable3d[Plane3d] with Scalable3d[Plane3d] {

  require(handedness.sign == Sign.of(xDirection.cross(yDirection).dot(normalDirection)))

  override def transformedBy(transformation: Transformation3d): Plane3d =
    Plane3d(
      originPoint.transformedBy(transformation),
      normalDirection.transformedBy(transformation),
      handedness.transformedBy(transformation),
      xDirection.transformedBy(transformation),
      yDirection.transformedBy(transformation)
    )

  override def scaledAbout(point: Point3d, scale: Double): Plane3d = {
    require(scale > 0.0)
    Plane3d(
      originPoint.scaledAbout(point, scale),
      normalDirection,
      handedness,
      xDirection,
      yDirection
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
    Plane3d(originPoint, normalDirection, handedness, xDirection, yDirection)
  }

  def fromBasisDirections(
    originPoint: Point3d,
    xDirection: Direction3d,
    yDirection: Direction3d
  ): Plane3d = Plane3d.fromBasisDirections(originPoint, xDirection, yDirection, Handedness.Right)

  def fromBasisDirections(
    originPoint: Point3d,
    xDirection: Direction3d,
    yDirection: Direction3d,
    handedness: Handedness
  ): Plane3d =
    Plane3d(
      originPoint,
      handedness.sign * Direction3d.fromComponents(xDirection.cross(yDirection).components),
      handedness,
      xDirection,
      yDirection
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
      handedness.sign * rightHandedNormalDirection,
      handedness,
      xDirection,
      yDirection
    );
  }

  def midplane(lowerPoint: Point3d, upperPoint: Point3d): Plane3d = {
    val displacementVector = upperPoint - lowerPoint
    Plane3d(lowerPoint + 0.5 * displacementVector, displacementVector.direction)
  }

  def midplane(lowerPlane: Plane3d, upperPlane: Plane3d): Plane3d = {
    val displacementVector = upperPlane.originPoint - lowerPlane.originPoint

    // Compute origin point equidistant from both planes
    val aboveDistance = upperPlane.originPoint.distanceTo(lowerPlane).abs
    val belowDistance = lowerPlane.originPoint.distanceTo(upperPlane).abs
    val distanceSum = belowDistance + aboveDistance
    val interpolationParameter =
      if (distanceSum == 0.0) 0.5 else belowDistance / (aboveDistance + belowDistance)
    val originPoint = lowerPlane.originPoint + interpolationParameter * displacementVector

    // Compute normal direction from the average of the two inputs, in the
    // direction lower -> upper
    val dotProductSign = Sign.of(lowerPlane.normalDirection.dot(upperPlane.normalDirection))
    val lowerNormalVector = lowerPlane.normalDirection.vector
    val upperNormalVector = dotProductSign * upperPlane.normalDirection.vector
    val normalVectorSum = lowerNormalVector + upperNormalVector
    val normalDirection =
      if (normalVectorSum.dot(displacementVector) >= 0.0) {
        normalVectorSum.direction
      } else {
        -normalVectorSum.direction
      }

    Plane3d(originPoint, normalDirection)
  }
}
