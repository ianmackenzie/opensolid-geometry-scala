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

final case class Plane3d(
  originPoint: Point3d,
  xDirection: Direction3d,
  yDirection: Direction3d,
  normalDirection: Direction3d
) extends Transformable3d[Plane3d] {

  def basisDirections: (Direction3d, Direction3d) =
    (xDirection, yDirection)

  override def transformedBy(transformation: Transformation3d): Plane3d =
    Plane3d(
      originPoint.transformedBy(transformation),
      xDirection.transformedBy(transformation),
      yDirection.transformedBy(transformation),
      normalDirection.transformedBy(transformation)
    )

  def translatedTo(point: Point3d): Plane3d =
    Plane3d(point, xDirection, yDirection, normalDirection)

  def offsetBy(distance: Double): Plane3d =
    translatedBy(distance * normalDirection)

  def flipped: Plane3d =
    Plane3d(originPoint, xDirection, yDirection, -normalDirection)

  def normalAxis: Axis3d =
    Axis3d(originPoint, normalDirection)

  def signedDistanceTo(point: Point3d): Double =
    originPoint.vectorTo(point).componentIn(normalDirection)

  def signedDistanceTo[P](expression: PointExpression3d[P]): ScalarExpression[P] =
    PointExpression3d.Constant(originPoint).vectorTo(expression).componentIn(normalDirection)
}

object Plane3d {
  def fromPointAndBasis(
    originPoint: Point3d,
    xDirection: Direction3d,
    yDirection: Direction3d
  ): Plane3d = {
    val normalDirection = Numerics.binormalToBasis(xDirection, yDirection)
    Plane3d(originPoint, xDirection, yDirection, normalDirection)
  }

  def fromPointAndNormal(originPoint: Point3d, normalDirection: Direction3d): Plane3d = {
    val (xDirection, yDirection) = Numerics.normalBasis(normalDirection)
    new Plane3d(originPoint, xDirection, yDirection, normalDirection)
  }

  def throughPoints(firstPoint: Point3d, secondPoint: Point3d, thirdPoint: Point3d): Plane3d = {
    val normalDirection = Numerics.normalDirection(firstPoint, secondPoint, thirdPoint)
    val xDirection = firstPoint.vectorTo(secondPoint) match {
      case Vector3d.Zero => normalDirection.normalDirection
      case nonZeroVector: Vector3d => nonZeroVector.direction
    }
    val yDirection = Numerics.binormalToBasis(normalDirection, xDirection)
    Plane3d(firstPoint, xDirection, yDirection, normalDirection)
  }

  def throughAxis(axis: Axis3d): Plane3d = {
    val xDirection = axis.direction
    val yDirection = axis.normalDirection
    val normalDirection = Numerics.binormalToBasis(xDirection, yDirection)
    Plane3d(axis.originPoint, xDirection, yDirection, normalDirection)
  }

  def throughAxisAndPoint(axis: Axis3d, point: Point3d): Plane3d = {
    val xDirection = axis.direction
    val crossProduct = xDirection.cross(axis.originPoint.vectorTo(point))
    val normalDirection = crossProduct match {
      case Vector3d.Zero => axis.normalDirection
      case nonZeroVector: Vector3d => nonZeroVector.direction
    }
    val yDirection = Numerics.binormalToBasis(normalDirection, xDirection)
    Plane3d(axis.originPoint, xDirection, yDirection, normalDirection)
  }

  def midplaneBetweenPoints(lowerPoint: Point3d, upperPoint: Point3d): Plane3d = {
    val displacementVector = lowerPoint.vectorTo(upperPoint)
    Plane3d.fromPointAndNormal(lowerPoint + 0.5 * displacementVector, displacementVector.direction)
  }

  def midplaneBetweenPlanes(lowerPlane: Plane3d, upperPlane: Plane3d): Plane3d = {
    val displacementVector = lowerPlane.originPoint.vectorTo(upperPlane.originPoint)

    // Compute origin point equidistant from both planes
    val aboveDistance = upperPlane.originPoint.distanceTo(lowerPlane).abs
    val belowDistance = lowerPlane.originPoint.distanceTo(upperPlane).abs
    val distanceSum = belowDistance + aboveDistance
    val interpolationParameter =
      if (distanceSum == 0.0) 0.5 else belowDistance / (aboveDistance + belowDistance)
    val originPoint = lowerPlane.originPoint + interpolationParameter * displacementVector

    // Compute normal direction from the average of the two inputs, in the
    // direction lower -> upper
    val lowerNormalVector = lowerPlane.normalDirection.vector
    val upperNormalVector = upperPlane.normalDirection.vector
    val normalVectorSum =
      if (lowerNormalVector.dot(upperNormalVector) >= 0.0) {
        lowerNormalVector + upperNormalVector
      } else {
        lowerNormalVector - upperNormalVector
      }
    val normalDirection =
      if (normalVectorSum.dot(displacementVector) >= 0.0) {
        normalVectorSum.direction
      } else {
        -normalVectorSum.direction
      }

    Plane3d.fromPointAndNormal(originPoint, normalDirection)
  }

  val XY: Plane3d = Frame3d.Global.xyPlane

  val XZ: Plane3d = Frame3d.Global.xzPlane

  val YX: Plane3d = Frame3d.Global.yxPlane

  val YZ: Plane3d = Frame3d.Global.yzPlane

  val ZX: Plane3d = Frame3d.Global.zxPlane

  val ZY: Plane3d = Frame3d.Global.zyPlane
}
