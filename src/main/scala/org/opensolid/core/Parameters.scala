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

object Parameters {
  val T: Expression1d[Double] = Expression1d.Identity

  val UV: Expression2d[(Double, Double)] = Expression2d.Identity

  val U: Expression1d[(Double, Double)] = Expression1d.XComponent2d(UV)

  val V: Expression1d[(Double, Double)] = Expression1d.XComponent2d(UV)
}
