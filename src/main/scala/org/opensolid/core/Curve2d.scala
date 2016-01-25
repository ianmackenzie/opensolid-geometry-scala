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

trait Curve2d {
  import Curve2d._

  def expression: Expression

  def domain: Interval

  def evaluate(parameterValue: Double): Point2d

  def evaluate(parameterBounds: Interval): Box2d

  def bounds: Box2d
}

object Curve2d {
  type Expression = Expression2d[CurveParameter]

  def apply(expression: Expression, domain: Interval): Curve2d = parametric(expression, domain)

  def parametric(expression: Expression, domain: Interval): Curve2d = Parametric(expression, domain)

  private case class Parametric(expression: Expression, domain: Interval) extends Curve2d {
    private[this] val (evaluationSequence, (xIndex, yIndex)) =
      EvaluationSequence.evaluate2d(expression)

    override def evaluate(parameterValue: Double): Point2d = {
      val array = Array.ofDim[Double](evaluationSequence.arraySize)
      array(0) = parameterValue
      for (operation <- evaluationSequence.operations) {
        operation.execute(array)
      }
      Point2d(array(xIndex), array(yIndex))
    }

    override def evaluate(parameterBounds: Interval): Box2d = {
      val array = Array.ofDim[Interval](evaluationSequence.arraySize)
      array(0) = parameterBounds
      for (operation <- evaluationSequence.operations) {
        operation.execute(array)
      }
      Box2d(array(xIndex), array(yIndex))
    }

    override val bounds: Box2d = evaluate(domain)
  }

  def constant(point: Point2d): Curve2d = Constant(point)

  private case class Constant(point: Point2d) extends Curve2d {
    private[this] val box = Box2d.singleton(point)

    override def expression: Expression = Expression2d.Constant(point.x, point.y)

    override def domain: Interval = Interval.Whole

    override def evaluate(parameterValue: Double): Point2d = point

    override def evaluate(parameterBounds: Interval): Box2d = box

    override def bounds: Box2d = box
  }
}
