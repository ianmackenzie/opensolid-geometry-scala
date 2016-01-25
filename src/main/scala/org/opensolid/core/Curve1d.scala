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

trait Curve1d {
  import Curve1d._

  def expression: Expression

  def domain: Interval

  def evaluate(parameterValue: Double): Double

  def bounds(parameterBounds: Interval): Interval
}

object Curve1d {
  type Expression = Expression1d[Double]

  def apply(expression: Expression, domain: Interval): Curve1d = new Generic(expression, domain)

  case class Constant(value: Double) extends Curve1d {
    private[this] val interval = Interval.singleton(value)

    override def expression: Expression = Expression1d.Constant(value)

    override def domain: Interval = Interval.Whole

    override def evaluate(parameterValue: Double): Double = value

    override def bounds(parameterBounds: Interval): Interval = interval
  }

  private class Generic(val expression: Expression, val domain: Interval) extends Curve1d {
    private[this] val (evaluationSequence, resultIndex) = EvaluationSequence.evaluate1d(expression)

    override def evaluate(parameterValue: Double): Double = {
      val array = Array.ofDim[Double](evaluationSequence.arraySize)
      array(0) = parameterValue
      for (operation <- evaluationSequence.operations) {
        operation.execute(array)
      }
      array(resultIndex)
    }

    override def bounds(parameterBounds: Interval): Interval = {
      val array = Array.ofDim[Interval](evaluationSequence.arraySize)
      array(0) = parameterBounds
      for (operation <- evaluationSequence.operations) {
        operation.execute(array)
      }
      array(resultIndex)
    }
  }
}
