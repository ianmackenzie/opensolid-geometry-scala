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

trait Curve2d extends Bounded[Bounds2d] {
  def parameterized: ParametricCurve2d

  def parameterizedBy(
    expression: Expression2d[CurveParameter],
    domain: Interval
  ): ParametricCurve2d =
    new ParametricCurve2d(expression, domain) {
      override def bounds: Bounds2d =
        Curve2d.this.bounds
    }
}
