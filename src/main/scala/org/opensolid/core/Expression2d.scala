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

sealed abstract class Expression2d[P] {
  import Expression2d._

  def derivative(parameter: P): VectorExpression2d[P]

  final def +(vectorExpression: VectorExpression2d[P]): Expression2d[P] =
    (this, vectorExpression) match {
      case (Constant(point), VectorExpression2d.Constant(vector)) => Constant(point + vector)
      case (expression, VectorExpression2d.Constant(Vector2d.Zero)) => expression
      case (first, VectorExpression2d.Negation(second)) => first - second
      case _ => PointPlusVector(this, vectorExpression)
    }

  final def +(vector: Vector2d): Expression2d[P] =
    this + VectorExpression2d.Constant[P](vector)

  final def plus(vectorExpression: VectorExpression2d[P]): Expression2d[P] =
    this + vectorExpression

  final def plus(vector: Vector2d): Expression2d[P] =
    this + VectorExpression2d.Constant[P](vector)

  final def -(vectorExpression: VectorExpression2d[P]): Expression2d[P] =
    (this, vectorExpression) match {
      case (Constant(point), VectorExpression2d.Constant(vector)) => Constant(point - vector)
      case (expression, VectorExpression2d.Constant(Vector2d.Zero)) => expression
      case (first, VectorExpression2d.Negation(second)) => first + second
      case _ => PointMinusVector(this, vectorExpression)
    }

  final def -(vector: Vector2d): Expression2d[P] =
    this - VectorExpression2d.Constant[P](vector)

  final def minus(vectorExpression: VectorExpression2d[P]): Expression2d[P] =
    this - vectorExpression

  final def minus(vector: Vector2d): Expression2d[P] =
    this - VectorExpression2d.Constant[P](vector)

  final def vectorTo(that: Expression2d[P]): VectorExpression2d[P] = (this, that) match {
    case (Constant(firstPoint), Constant(secondPoint)) =>
      VectorExpression2d.Constant(firstPoint.vectorTo(secondPoint))
    case (first, second) if (first == second) =>
      VectorExpression2d.Constant(Vector2d.Zero)
    case _ =>
      VectorExpression2d.Displacement(this, that)
  }

  final def vectorTo(point: Point2d): VectorExpression2d[P] =
    this.vectorTo(Constant[P](point))

  def x: Expression1d[P] =
    Expression1d.PointXComponent2d(this)

  def y: Expression1d[P] =
    Expression1d.PointYComponent2d(this)

  final def squaredDistanceTo(that: Expression2d[P]): Expression1d[P] =
    vectorTo(that).squaredLength

  final def squaredDistanceTo(point: Point2d): Expression1d[P] =
    squaredDistanceTo(Constant[P](point))

  final def distanceTo(that: Expression2d[P]): Expression1d[P] =
    vectorTo(that).length

  final def distanceTo(point: Point2d): Expression1d[P] =
    distanceTo(Constant[P](point))
}

object Expression2d {
  def fromComponents[P](
    xExpression: Expression1d[P],
    yExpression: Expression1d[P]
  ): Expression2d[P] = (xExpression, yExpression) match {
    case (Expression1d.Constant(xValue), Expression1d.Constant(yValue)) =>
      Constant(Point2d(xValue, yValue))
    case _ => FromComponents(xExpression, yExpression)
  }

  case class Constant[P](val point: Point2d) extends Expression2d[P] {
    override def derivative(parameter: P): VectorExpression2d[P] =
      VectorExpression2d.Constant(Vector2d.Zero)

    override def x: Expression1d[P] =
      Expression1d.Constant(point.x)

    override def y: Expression1d[P] =
      Expression1d.Constant(point.y)
  }

  case class FromComponents[P](
    override val x: Expression1d[P],
    override val y: Expression1d[P]
  ) extends Expression2d[P] {

    override def derivative(parameter: P): VectorExpression2d[P] =
      VectorExpression2d.fromComponents(x.derivative(parameter), y.derivative(parameter))
  }

  case class PointPlusVector[P](
    pointExpression: Expression2d[P],
    vectorExpression: VectorExpression2d[P]
  ) extends Expression2d[P] {

    override def derivative(parameter: P): VectorExpression2d[P] =
      pointExpression.derivative(parameter) + vectorExpression.derivative(parameter)

    override def x: Expression1d[P] =
      pointExpression.x + vectorExpression.x

    override def y: Expression1d[P] =
      pointExpression.y + vectorExpression.y
  }

  case class PointMinusVector[P](
    pointExpression: Expression2d[P],
    vectorExpression: VectorExpression2d[P]
  ) extends Expression2d[P] {

    override def derivative(parameter: P): VectorExpression2d[P] =
      pointExpression.derivative(parameter) - vectorExpression.derivative(parameter)

    override def x: Expression1d[P] =
      pointExpression.x - vectorExpression.x

    override def y: Expression1d[P] =
      pointExpression.y - vectorExpression.y
  }
}
