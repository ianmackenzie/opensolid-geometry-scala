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

import org.opensolid.core.*;
import org.junit.Test;

public class ScalarTests {
  @Test
  public void run() {
    Scalar scalar = new Scalar(2.0);

    boolean isZero = scalar.isZero(1e-3);
    boolean isNotZero = scalar.isNotZero(1e-3);

    double meters = scalar.meters();
    double inMeters = scalar.inMeters();
    double centimeters = scalar.centimeters();
    double inCentimeters = scalar.inCentimeters();
    double millimeters = scalar.millimeters();
    double inMillimeters = scalar.inMillimeters();
    double microns = scalar.microns();
    double inMicrons = scalar.inMicrons();
    double kilometers = scalar.kilometers();
    double inKilometers = scalar.inKilometers();
    double inches = scalar.inches();
    double inInches = scalar.inInches();
    double feet = scalar.feet();
    double inFeet = scalar.inFeet();
    double thou = scalar.thou();
    double inThou = scalar.inThou();
    double yards = scalar.yards();
    double inYards = scalar.inYards();
    double miles = scalar.miles();
    double inMiles = scalar.inMiles();
    double radians = scalar.radians();
    double inRadians = scalar.inRadians();
    double degrees = scalar.degrees();
    double inDegrees = scalar.inDegrees();

    double timesSign = scalar.times(Sign.Negative());
    Interval plusInterval = scalar.plus(Interval.Unit());
    Interval minusInterval = scalar.minus(Interval.Unit());
    Interval timesInterval = scalar.times(Interval.Unit());
    Interval dividedByInterval = scalar.dividedBy(Interval.Unit());
    Vector2d timesVector2d = scalar.times(Vector2d.Zero());
    Vector3d timesVector3d = scalar.times(Vector3d.Zero());
    Vector2d timesDirection2d = scalar.times(Direction2d.X());
    Vector3d timesDirection3d = scalar.times(Direction3d.Y());

    Interval hullDouble = scalar.hull(2.0);
    Interval hullInterval = scalar.hull(Interval.Unit());
  }
}
