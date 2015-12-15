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

package org.opensolid.junit;

import org.opensolid.core.*;
import org.junit.Test;

public class ScalarTests {
  @Test
  public void run() {
    double tolerance = 1e-3;
    boolean isZero = Scalar.isZero(1e-4, tolerance);
    boolean isNotZero = Scalar.isNotZero(1e-2, tolerance);
    Interval difference = Scalar.difference(1.0, new Interval(2.0, 3.0));
    Interval quotient = Scalar.quotient(1.0, new Interval(2.0, 3.0));
    Interval hull = Scalar.hull(3.0, 2.0);
  }
}
