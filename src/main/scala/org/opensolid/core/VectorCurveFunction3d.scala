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

trait VectorCurveFunction3d extends Function1[Double, Vector3d] {
  def apply(interval: Interval): VectorBounds3d
}

object VectorCurveFunction3d {
  def compile(expression: VectorExpression3d[CurveParameter]): VectorCurveFunction3d = {
    val compiler = new ExpressionCompiler(1)
    val (xIndex, yIndex, zIndex) = compiler.evaluate(expression)
    val arrayOperations = compiler.arrayOperations.toArray
    val arraySize = compiler.arraySize

    new VectorCurveFunction3d {
      override def apply(parameterValue: Double): Vector3d = {
        val array = Array.ofDim[Double](arraySize)
        array(0) = parameterValue
        for (operation <- arrayOperations) {
          operation.execute(array)
        }
        Vector3d(array(xIndex), array(yIndex), array(zIndex))
      }

      override def apply(parameterBounds: Interval): VectorBounds3d = {
        val array = Array.ofDim[Interval](arraySize)
        array(0) = parameterBounds
        for (operation <- arrayOperations) {
          operation.execute(array)
        }
        VectorBounds3d(array(xIndex), array(yIndex), array(zIndex))
      }
    }
  }
}
