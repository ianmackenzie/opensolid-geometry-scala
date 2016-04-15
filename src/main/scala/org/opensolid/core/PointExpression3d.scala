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

sealed abstract class PointExpression3d[P] {
  import PointExpression3d._

  def derivative(parameter: P): VectorExpression3d[P]

  final def +(vectorExpression: VectorExpression3d[P]): PointExpression3d[P] =
    (this, vectorExpression) match {
      case (Constant(point), VectorExpression3d.Constant(vector)) => Constant(point + vector)
      case (expression, VectorExpression3d.Constant(Vector3d.Zero)) => expression
      case (first, VectorExpression3d.Negation(second)) => first - second
      case _ => PointPlusVector(this, vectorExpression)
    }

  final def +(vector: Vector3d): PointExpression3d[P] =
    this + VectorExpression3d.Constant[P](vector)

  final def plus(vectorExpression: VectorExpression3d[P]): PointExpression3d[P] =
    this + vectorExpression

  final def plus(vector: Vector3d): PointExpression3d[P] =
    this + VectorExpression3d.Constant[P](vector)

  final def -(vectorExpression: VectorExpression3d[P]): PointExpression3d[P] =
    (this, vectorExpression) match {
      case (Constant(point), VectorExpression3d.Constant(vector)) => Constant(point - vector)
      case (expression, VectorExpression3d.Constant(Vector3d.Zero)) => expression
      case (first, VectorExpression3d.Negation(second)) => first + second
      case _ => PointMinusVector(this, vectorExpression)
    }

  final def -(vector: Vector3d): PointExpression3d[P] =
    this - VectorExpression3d.Constant[P](vector)

  final def minus(vectorExpression: VectorExpression3d[P]): PointExpression3d[P] =
    this - vectorExpression

  final def minus(vector: Vector3d): PointExpression3d[P] =
    this - VectorExpression3d.Constant[P](vector)

  final def vectorTo(that: PointExpression3d[P]): VectorExpression3d[P] = (this, that) match {
    case (Constant(firstPoint), Constant(secondPoint)) =>
      VectorExpression3d.Constant(firstPoint.vectorTo(secondPoint))
    case (first, second) if (first == second) =>
      VectorExpression3d.Constant(Vector3d.Zero)
    case _ =>
      VectorExpression3d.Displacement(this, that)
  }

  final def vectorTo(point: Point3d): VectorExpression3d[P] =
    this.vectorTo(Constant[P](point))

  def x: Expression1d[P] =
    Expression1d.PointXComponent3d(this)

  def y: Expression1d[P] =
    Expression1d.PointYComponent3d(this)

  def z: Expression1d[P] =
    Expression1d.PointZComponent3d(this)

  final def squaredDistanceTo(that: PointExpression3d[P]): Expression1d[P] =
    vectorTo(that).squaredLength

  final def squaredDistanceTo(point: Point3d): Expression1d[P] =
    squaredDistanceTo(Constant[P](point))

  final def distanceTo(that: PointExpression3d[P]): Expression1d[P] =
    vectorTo(that).length

  final def distanceTo(point: Point3d): Expression1d[P] =
    distanceTo(Constant[P](point))
}

object PointExpression3d {
  def fromComponents[P](
    xExpression: Expression1d[P],
    yExpression: Expression1d[P],
    zExpression: Expression1d[P]
  ): PointExpression3d[P] = (xExpression, yExpression, zExpression) match {
    case (
      Expression1d.Constant(xValue),
      Expression1d.Constant(yValue),
      Expression1d.Constant(zValue)
    ) => Constant(Point3d(xValue, yValue, zValue))
    case _ => FromComponents(xExpression, yExpression, zExpression)
  }

  case class Constant[P](val point: Point3d) extends PointExpression3d[P] {
    override def derivative(parameter: P): VectorExpression3d[P] =
      VectorExpression3d.Constant(Vector3d.Zero)

    override def x: Expression1d[P] =
      Expression1d.Constant(point.x)

    override def y: Expression1d[P] =
      Expression1d.Constant(point.y)

    override def z: Expression1d[P] =
      Expression1d.Constant(point.z)
  }

  case class FromComponents[P](
    override val x: Expression1d[P],
    override val y: Expression1d[P],
    override val z: Expression1d[P]
  ) extends PointExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      VectorExpression3d.fromComponents(
        x.derivative(parameter),
        y.derivative(parameter),
        z.derivative(parameter)
      )
  }

  case class PointPlusVector[P](
    pointExpression: PointExpression3d[P],
    vectorExpression: VectorExpression3d[P]
  ) extends PointExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      pointExpression.derivative(parameter) + vectorExpression.derivative(parameter)

    override def x: Expression1d[P] =
      pointExpression.x + vectorExpression.x

    override def y: Expression1d[P] =
      pointExpression.y + vectorExpression.y

    override def z: Expression1d[P] =
      pointExpression.z + vectorExpression.z
  }

  case class PointMinusVector[P](
    pointExpression: PointExpression3d[P],
    vectorExpression: VectorExpression3d[P]
  ) extends PointExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      pointExpression.derivative(parameter) - vectorExpression.derivative(parameter)

    override def x: Expression1d[P] =
      pointExpression.x - vectorExpression.x

    override def y: Expression1d[P] =
      pointExpression.y - vectorExpression.y

    override def z: Expression1d[P] =
      pointExpression.z - vectorExpression.z
  }
}
