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

class ParametricCurve2d(
  val expression: PointExpression2d[Double],
  val domain: Interval
) extends Curve2d {

  private[this] val (arrayOperations, arraySize, (xIndex, yIndex)) =
    ExpressionCompiler.compile(expression)

  override def bounds: Bounds2d =
    evaluate(domain)

  override def parameterized: ParametricCurve2d =
    this

  def evaluate(parameterValue: Double): Point2d = {
    val array = Array.ofDim[Double](arraySize)
    array(0) = parameterValue
    for { operation <- arrayOperations } operation.execute(array)
    Point2d(array(xIndex), array(yIndex))
  }

  def evaluate(parameterBounds: Interval): Bounds2d = {
    val array = Array.ofDim[Interval](arraySize)
    array(0) = parameterBounds
    for { operation <- arrayOperations } operation.execute(array)
    Bounds2d(array(xIndex), array(yIndex))
  }
}
