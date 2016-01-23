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

object ExpressionCompiler {
  def compileCurve1d(expression: Expression1d[Double]): (Double) => Double = ???

  def compileSurface1d(expression: Expression1d[Point2d]): (Point2d) => Double = ???

  def compileCurve2d(expression: Expression2d[Double]): (Double) => Point2d = ???

  def compileVectorCurve2d(expression: Expression2d[Double]): (Double) => Vector2d = ???

  def compileDirectionCurve2d(expression: Expression2d[Double]): (Double) => Direction2d = ???

  // def compileCurve3d(expression: Expression3d[Double]): (Double) => Point3d = ???

  // def compileVectorCurve3d(expression: Expression3d[Double]): (Double) => Vector3d = ???

  // def compileDirectionCurve3d(expression: Expression3d[Double]): (Double) => Direction3d = ???

  // def compileSurface3d(expression: Expression3d[Point2d]): (Point2d) => Point3d = ???

  // def compileVectorSurface3d(expression: Expression3d[Point2d]): (Point2d) => Vector3d = ???

  // def compileDirectionSurface3d(expression: Expression3d[Point2d]): (Point2d) => Direction3d = ???
}
