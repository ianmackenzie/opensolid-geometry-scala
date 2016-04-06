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

import org.scalatest._
import org.scalatest.prop._

abstract class TestSuite extends FunSuite
  with Matchers
  with PropertyChecks
  with DoubleMatchers
  with DoubleGenerators {

  import TestSuite._

  def eps(value: Double): Double =
    math.ulp(value).max(minEps).min(maxEps)

  def eps(interval: Interval): Double =
    interval.ulp.max(minEps).min(maxEps)
}

object TestSuite {
  val minEps: Double = math.ulp(1.0)

  val maxEps: Double = Double.MaxValue
}
