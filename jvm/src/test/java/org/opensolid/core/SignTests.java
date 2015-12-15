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

public class SignTests {
  @Test
  public void run() {
    Sign positive = Sign.getPositive();
    Sign negative = Sign.getNegative();
    Sign none = Sign.getNone();
    Sign ofDouble = Sign.of(-3.0);

    int value = positive.value();

    Sign negated = positive.negated();
    Sign timesSign = positive.times(negative);
    double timesDouble = negative.times(3.0);
    Interval timesInterval = positive.times(Interval.getUnit());
    Handedness timesHandedness = negative.times(Handedness.getLeft());
    Vector2d timesVector2d = positive.times(Vector2d.getZero());
    Vector3d timesVector3d = negative.times(Vector3d.getZero());
    Direction2d timesDirection2d = positive.times(Direction2d.getY());
    Direction3d timesDirection3d = negative.times(Direction3d.getZ());
    VectorBoundingBox2d timesVectorBoundingBox2d = positive.times(VectorBoundingBox2d.getEmpty());
    VectorBoundingBox3d timesVectorBoundingBox3d = negative.times(VectorBoundingBox3d.getEmpty());
    DirectionBoundingBox2d timesDirectionBoundingBox2d =
        positive.times(DirectionBoundingBox2d.getEmpty());
    DirectionBoundingBox3d timesDirectionBoundingBox3d =
        negative.times(DirectionBoundingBox3d.getEmpty());
  }
}
