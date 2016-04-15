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

trait CurveFunction1d extends Function1[Double, Double] {
  def apply(interval: Interval): Interval
}

object CurveFunction1d {
  def compile(expression: ScalarExpression[CurveParameter]): CurveFunction1d = {
    val compiler = new ExpressionCompiler(1)
    val resultIndex = compiler.evaluate(expression)
    val arrayOperations = compiler.arrayOperations.toArray
    val arraySize = compiler.arraySize

    new CurveFunction1d {
      override def apply(parameterValue: Double): Double = {
        val array = Array.ofDim[Double](arraySize)
        array(0) = parameterValue
        for (operation <- arrayOperations) {
          operation.execute(array)
        }
        array(resultIndex)
      }

      override def apply(parameterBounds: Interval): Interval = {
        val array = Array.ofDim[Interval](arraySize)
        array(0) = parameterBounds
        for (operation <- arrayOperations) {
          operation.execute(array)
        }
        array(resultIndex)
      }
    }
  }
}
