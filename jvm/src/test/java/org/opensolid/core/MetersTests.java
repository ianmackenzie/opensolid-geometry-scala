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

public class MetersTests {
  @Test
  public void run() {
    double fromCentimeters = Meters.fromCentimeters(1.0);
    double toCentimeters = Meters.toCentimeters(1.0);
    double fromMillimeters = Meters.fromMillimeters(1.0);
    double toMillimeters = Meters.toMillimeters(1.0);
    double fromMicrons = Meters.fromMicrons(1.0);
    double toMicrons = Meters.toMicrons(1.0);
    double fromKilometers = Meters.fromKilometers(1.0);
    double fromToKilometers = Meters.fromToKilometers(1.0);
    double fromInches = Meters.fromInches(1.0);
    double toInches = Meters.toInches(1.0);
    double fromFeet = Meters.fromFeet(1.0);
    double toFeet = Meters.toFeet(1.0);
    double fromThou = Meters.fromThou(1.0);
    double toThou = Meters.toThou(1.0);
    double fromYards = Meters.fromYards(1.0);
    double toYards = Meters.toYards(1.0);
    double fromMiles = Meters.fromMiles(1.0);
    double toMiles = Meters.toMiles(1.0);
  }
}
