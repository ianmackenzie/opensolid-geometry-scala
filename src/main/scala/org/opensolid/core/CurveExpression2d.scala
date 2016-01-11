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

abstract class CurveExpression2d {
  def evaluate(t: Double): Point2d

  def tangentDirection: DirectionCurveExpression2d

  def normalDirection: DirectionCurveExpression2d

  def curvature: CurveExpression1d
}

object CurveExpression2d {
  case class Constant(point: Point2d) extends CurveExpression2d {
    def domain: Interval = Interval.Whole

    def evaluate(t: Double): Point2d = point

    def tangentDirection: DirectionCurveExpression2d =
      DirectionCurveExpression2d.Constant(Direction2d.None)

    def normalDirection: DirectionCurveExpression2d =
      DirectionCurveExpression2d.Constant(Direction2d.None)

    def curvature: CurveExpression1d = CurveExpression1d.Constant(0.0)
  }
}
