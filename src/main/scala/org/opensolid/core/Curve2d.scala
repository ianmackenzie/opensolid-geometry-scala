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

abstract class Curve2d {
  def domain: Interval

  def evaluate(t: Double): Point2d

  def tangentDirection: DirectionCurve2d

  def normalDirection: DirectionCurve2d

  def curvature: Curve1d
}

object Curve2d {
  case class Constant(point: Point2d) extends Curve2d {
    def domain: Interval = Interval.Whole

    def evaluate(t: Double): Point2d = point

    def tangentDirection: DirectionCurve2d = DirectionCurve2d.Constant(Direction2d.None)

    def normalDirection: DirectionCurve2d = DirectionCurve2d.Constant(Direction2d.None)

    def curvature: Curve1d = Curve1d.Constant(0.0)
  }
}
