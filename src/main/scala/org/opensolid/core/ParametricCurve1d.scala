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

class ParametricCurve1d(
  val expression: ScalarExpression[CurveParameter],
  val domain: Interval
) extends Curve1d {

  val function: CurveFunction1d = CurveFunction1d.compile(expression)

  override def bounds: Interval =
    function(domain)

  override def parameterized: ParametricCurve1d =
    this

  def evaluateAt(parameterValue: Double): Double =
    function(parameterValue)
}

object ParametricCurve1d {
  def apply(expression: ScalarExpression[CurveParameter], domain: Interval): ParametricCurve1d =
    new ParametricCurve1d(expression, domain)
}
