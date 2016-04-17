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

trait CurveFunction3d extends Function1[Double, Point3d] {
  def apply(interval: Interval): Bounds3d
}

object CurveFunction3d {
  def compile(expression: Expression3d[CurveParameter]): CurveFunction3d = {
    val compiler = new ExpressionCompiler(1)
    val (xIndex, yIndex, zIndex) = compiler.evaluate(expression)
    val arrayOperations = compiler.arrayOperations.toArray
    val arraySize = compiler.arraySize

    new CurveFunction3d {
      override def apply(parameterValue: Double): Point3d = {
        val array = Array.ofDim[Double](arraySize)
        array(0) = parameterValue
        arrayOperations.foreach(_.execute(array))
        Point3d(array(xIndex), array(yIndex), array(zIndex))
      }

      override def apply(parameterBounds: Interval): Bounds3d = {
        val array = Array.ofDim[Interval](arraySize)
        array(0) = parameterBounds
        arrayOperations.foreach(_.execute(array))
        Bounds3d(array(xIndex), array(yIndex), array(zIndex))
      }
    }
  }
}
