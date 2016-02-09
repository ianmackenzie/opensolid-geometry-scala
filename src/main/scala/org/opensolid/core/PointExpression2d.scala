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

sealed abstract class PointExpression2d[P] {
  import PointExpression2d._

  def derivative(parameter: P): VectorExpression2d[P]

  def condition: ScalarExpression[P]

  final def +(vectorExpression: VectorExpression2d[P]): PointExpression2d[P] =
    (this, vectorExpression) match {
      case (Constant(point), VectorExpression2d.Constant(vector)) => Constant(point + vector)
      case (expression, VectorExpression2d.Constant(Vector2d.Zero)) => expression
      case (first, VectorExpression2d.Negation(second)) => first - second
      case _ => PointPlusVector(this, vectorExpression)
    }

  final def +(vector: Vector2d): PointExpression2d[P] =
    this + VectorExpression2d.Constant[P](vector)

  final def plus(vectorExpression: VectorExpression2d[P]): PointExpression2d[P] =
    this + vectorExpression

  final def plus(vector: Vector2d): PointExpression2d[P] =
    this + VectorExpression2d.Constant[P](vector)

  final def -(vectorExpression: VectorExpression2d[P]): PointExpression2d[P] =
    (this, vectorExpression) match {
      case (Constant(point), VectorExpression2d.Constant(vector)) => Constant(point - vector)
      case (expression, VectorExpression2d.Constant(Vector2d.Zero)) => expression
      case (first, VectorExpression2d.Negation(second)) => first + second
      case _ => PointMinusVector(this, vectorExpression)
    }

  final def -(vector: Vector2d): PointExpression2d[P] =
    this - VectorExpression2d.Constant[P](vector)

  final def minus(vectorExpression: VectorExpression2d[P]): PointExpression2d[P] =
    this - vectorExpression

  final def minus(vector: Vector2d): PointExpression2d[P] =
    this - VectorExpression2d.Constant[P](vector)

  final def -(that: PointExpression2d[P]): VectorExpression2d[P] = (this, that) match {
    case (Constant(firstPoint), Constant(secondPoint)) =>
      VectorExpression2d.Constant(firstPoint - secondPoint)
    case (first, second) if (first == second) => VectorExpression2d.Constant(Vector2d.Zero)
    case _ => VectorExpression2d.PointDifference(this, that)
  }

  final def -(point: Point2d): VectorExpression2d[P] =
    this - Constant[P](point)

  final def minus(that: PointExpression2d[P]): VectorExpression2d[P] =
    this - that

  final def minus(point: Point2d): VectorExpression2d[P] =
    this - Constant[P](point)

  def x: ScalarExpression[P] =
    ScalarExpression.PointXComponent2d(this)

  def y: ScalarExpression[P] =
    ScalarExpression.PointYComponent2d(this)
}

object PointExpression2d {
  def fromComponents[P](
    xExpression: ScalarExpression[P],
    yExpression: ScalarExpression[P]
  ): PointExpression2d[P] = (xExpression, yExpression) match {
    case (ScalarExpression.Constant(xValue), ScalarExpression.Constant(yValue)) =>
      Constant(Point2d(xValue, yValue))
    case _ => FromComponents(xExpression, yExpression)
  }

  case class Constant[P](val point: Point2d) extends PointExpression2d[P] {
    override def derivative(parameter: P): VectorExpression2d[P] =
      VectorExpression2d.Constant(Vector2d.Zero)

    override def condition: ScalarExpression[P] =
      ScalarExpression.Constant(1)

    override def x: ScalarExpression[P] =
      ScalarExpression.Constant(point.x)

    override def y: ScalarExpression[P] =
      ScalarExpression.Constant(point.y)
  }

  case class FromComponents[P](
    override val x: ScalarExpression[P],
    override val y: ScalarExpression[P]
  ) extends PointExpression2d[P] {

    override def derivative(parameter: P): VectorExpression2d[P] =
      VectorExpression2d.fromComponents(x.derivative(parameter), y.derivative(parameter))

    override def condition: ScalarExpression[P] =
      x.condition * y.condition
  }

  case class PointPlusVector[P](
    pointExpression: PointExpression2d[P],
    vectorExpression: VectorExpression2d[P]
  ) extends PointExpression2d[P] {

    override def derivative(parameter: P): VectorExpression2d[P] =
      pointExpression.derivative(parameter) + vectorExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      pointExpression.condition * vectorExpression.condition

    override def x: ScalarExpression[P] =
      pointExpression.x + vectorExpression.x

    override def y: ScalarExpression[P] =
      pointExpression.y + vectorExpression.y
  }

  case class PointMinusVector[P](
    pointExpression: PointExpression2d[P],
    vectorExpression: VectorExpression2d[P]
  ) extends PointExpression2d[P] {

    override def derivative(parameter: P): VectorExpression2d[P] =
      pointExpression.derivative(parameter) - vectorExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      pointExpression.condition * vectorExpression.condition

    override def x: ScalarExpression[P] =
      pointExpression.x - vectorExpression.x

    override def y: ScalarExpression[P] =
      pointExpression.y - vectorExpression.y
  }
}