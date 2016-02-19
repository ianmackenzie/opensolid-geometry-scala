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

  def condition: ScalarExpression[P]

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

  final def -(that: PointExpression3d[P]): VectorExpression3d[P] = (this, that) match {
    case (Constant(firstPoint), Constant(secondPoint)) =>
      VectorExpression3d.Constant(firstPoint - secondPoint)
    case (first, second) if (first == second) => VectorExpression3d.Constant(Vector3d.Zero)
    case _ => VectorExpression3d.PointDifference(this, that)
  }

  final def -(point: Point3d): VectorExpression3d[P] =
    this - Constant[P](point)

  final def minus(that: PointExpression3d[P]): VectorExpression3d[P] =
    this - that

  final def minus(point: Point3d): VectorExpression3d[P] =
    this - Constant[P](point)

  def x: ScalarExpression[P] =
    ScalarExpression.PointXComponent3d(this)

  def y: ScalarExpression[P] =
    ScalarExpression.PointYComponent3d(this)

  def z: ScalarExpression[P] =
    ScalarExpression.PointZComponent3d(this)

  final def squaredDistanceTo(that: PointExpression3d[P]): ScalarExpression[P] =
    (this - that).squaredLength

  final def distanceTo(that: PointExpression3d[P]): ScalarExpression[P] =
    (this - that).length
}

object PointExpression3d {
  def fromComponents[P](
    xExpression: ScalarExpression[P],
    yExpression: ScalarExpression[P],
    zExpression: ScalarExpression[P]
  ): PointExpression3d[P] = (xExpression, yExpression, zExpression) match {
    case (
      ScalarExpression.Constant(xValue),
      ScalarExpression.Constant(yValue),
      ScalarExpression.Constant(zValue)
    ) => Constant(Point3d(xValue, yValue, zValue))
    case _ => FromComponents(xExpression, yExpression, zExpression)
  }

  case class CompiledCurve(evaluate: (Double) => Point3d, evaluateBounds: (Interval) => Bounds3d)

  def compile[P <: CurveParameter : OneDimensional](
    expression: PointExpression3d[P]
  ): CompiledCurve = {
    val compiler = new ExpressionCompiler(1)
    val (xIndex, yIndex, zIndex) = compiler.evaluate(expression)
    val arrayOperations = compiler.arrayOperations.toArray
    val arraySize = compiler.arraySize
    val evaluate = (parameterValue: Double) => {
      val array = Array.ofDim[Double](arraySize)
      array(0) = parameterValue
      for { operation <- arrayOperations } operation.execute(array)
      Point3d(array(xIndex), array(yIndex), array(zIndex))
    }
    val evaluateBounds = (parameterBounds: Interval) => {
      val array = Array.ofDim[Interval](arraySize)
      array(0) = parameterBounds
      for { operation <- arrayOperations } operation.execute(array)
      Bounds3d(array(xIndex), array(yIndex), array(zIndex))
    }
    CompiledCurve(evaluate, evaluateBounds)
  }

  case class CompiledSurface(evaluate: (Point2d) => Point3d, evaluateBounds: (Bounds2d) => Bounds3d)

  def compile[P <: SurfaceParameter : TwoDimensional](
    expression: PointExpression3d[P]
  ): CompiledSurface = {
    val compiler = new ExpressionCompiler(2)
    val (xIndex, yIndex, zIndex) = compiler.evaluate(expression)
    val arrayOperations = compiler.arrayOperations.toArray
    val arraySize = compiler.arraySize
    val evaluate = (parameterValue: Point2d) => {
      val array = Array.ofDim[Double](arraySize)
      array(0) = parameterValue.x
      array(1) = parameterValue.y
      for { operation <- arrayOperations } operation.execute(array)
      Point3d(array(xIndex), array(yIndex), array(zIndex))
    }
    val evaluateBounds = (parameterBounds: Bounds2d) => {
      val array = Array.ofDim[Interval](arraySize)
      array(0) = parameterBounds.x
      array(1) = parameterBounds.y
      for { operation <- arrayOperations } operation.execute(array)
      Bounds3d(array(xIndex), array(yIndex), array(zIndex))
    }
    CompiledSurface(evaluate, evaluateBounds)
  }

  case class Constant[P](val point: Point3d) extends PointExpression3d[P] {
    override def derivative(parameter: P): VectorExpression3d[P] =
      VectorExpression3d.Constant(Vector3d.Zero)

    override def condition: ScalarExpression[P] =
      ScalarExpression.Constant(1)

    override def x: ScalarExpression[P] =
      ScalarExpression.Constant(point.x)

    override def y: ScalarExpression[P] =
      ScalarExpression.Constant(point.y)

    override def z: ScalarExpression[P] =
      ScalarExpression.Constant(point.z)
  }

  case class FromComponents[P](
    override val x: ScalarExpression[P],
    override val y: ScalarExpression[P],
    override val z: ScalarExpression[P]
  ) extends PointExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      VectorExpression3d.fromComponents(
        x.derivative(parameter),
        y.derivative(parameter),
        z.derivative(parameter)
      )

    override def condition: ScalarExpression[P] =
      x.condition * y.condition * z.condition
  }

  case class PointPlusVector[P](
    pointExpression: PointExpression3d[P],
    vectorExpression: VectorExpression3d[P]
  ) extends PointExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      pointExpression.derivative(parameter) + vectorExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      pointExpression.condition * vectorExpression.condition

    override def x: ScalarExpression[P] =
      pointExpression.x + vectorExpression.x

    override def y: ScalarExpression[P] =
      pointExpression.y + vectorExpression.y

    override def z: ScalarExpression[P] =
      pointExpression.z + vectorExpression.z
  }

  case class PointMinusVector[P](
    pointExpression: PointExpression3d[P],
    vectorExpression: VectorExpression3d[P]
  ) extends PointExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      pointExpression.derivative(parameter) - vectorExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      pointExpression.condition * vectorExpression.condition

    override def x: ScalarExpression[P] =
      pointExpression.x - vectorExpression.x

    override def y: ScalarExpression[P] =
      pointExpression.y - vectorExpression.y

    override def z: ScalarExpression[P] =
      pointExpression.z - vectorExpression.z
  }
}
