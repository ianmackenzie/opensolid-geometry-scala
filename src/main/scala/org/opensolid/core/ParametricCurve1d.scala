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

case class ParametricCurve1d(
  expression: Expression1d[Expression1d.CurveParameter],
  domain: Interval
) extends Curve1d {

  override val bounds: Interval = evaluate(domain)

  override def parameterized: ParametricCurve1d = this

  private[this] val (arrayOperations, arraySize, resultIndex) =
    ExpressionCompiler.compile(expression)

  def evaluate(parameterValue: Double): Double = {
    val array = Array.ofDim[Double](arraySize)
    array(0) = parameterValue
    for { operation <- arrayOperations } operation.execute(array)
    array(resultIndex)
  }

  def evaluate(parameterBounds: Interval): Interval = {
    val array = Array.ofDim[Interval](arraySize)
    array(0) = parameterBounds
    for { operation <- arrayOperations } operation.execute(array)
    array(resultIndex)
  }
}
