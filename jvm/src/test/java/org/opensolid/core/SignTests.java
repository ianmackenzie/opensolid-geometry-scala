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

package org.opensolid.core;

import org.junit.Test;

public class SignTests {
  @Test
  public void run() {
    Sign positive = Sign.Positive();
    Sign negative = Sign.Negative();
    Sign none = Sign.None();
    Sign ofDouble = Sign.of(-3.0);

    int value = positive.value();

    Sign negated = positive.negated();
    Sign timesSign = positive.times(negative);
    double timesDouble = negative.times(3.0);
    Interval timesInterval = positive.times(new Interval(2.0, 3.0));
  }
}
