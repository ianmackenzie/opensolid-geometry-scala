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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import org.junit.Test;
import scala.Tuple2;

public class IntervalTests {
  @Test
  public void run() {
    Interval first = new Interval(2, 3);
    Interval second = new Interval(4, 5);
    Interval singleton = new Interval(6);
    assertEquals(first.lowerBound(), 2.0, 1e-12);
    assertEquals(first.upperBound(), 3.0, 1e-12);
    assertEquals(first, new Interval(first.lowerBound(), first.upperBound()));
    assertNotEquals(first, second);
    assertNotEquals(first.hashCode(), second.hashCode());
    assertNotEquals(first.toString(), second.toString());

    assertEquals(first.bounds(), first);
    assertFalse(first.isEmpty());
    assertFalse(first.isWhole());
    assertEquals(first.width(), 1.0, 1e-12);
    assertEquals(first.interpolated(0.5), 2.5, 1e-12);
    assertEquals(first.median(), 2.5, 1e-12);

    double randomValue = first.randomValue();
    assertTrue(randomValue >= first.lowerBound() - 1e-12);
    assertTrue(randomValue <= first.upperBound() - 1e-12);

    assertFalse(first.isSingleton());
    assertTrue(singleton.isSingleton());

    Tuple2<Interval, Interval> bisected = first.bisected();
    assertEquals(bisected._1(), new Interval(2, 2.5));
    assertEquals(bisected._2(), new Interval(2.5, 3));

    assertEquals(first.hull(1), new Interval(1, 3));
    assertEquals(first.hull(second), new Interval(2, 5));
    assertEquals(first.intersection(second), Interval.Empty());

    assertTrue(Interval.Whole().contains(1e6));
    assertTrue(first.contains(1.999, 1e-2));
    assertFalse(first.contains(1.999, 1e-6));
    assertTrue(first.hull(second).contains(first));
    assertTrue(first.hull(second).contains(first, 1e-3));
    assertFalse(first.hull(second).contains(first, -1e-3));
    assertFalse(first.overlaps(second));
    assertTrue(first.overlaps(second, 2));

    assertEquals(first.negated(), new Interval(-3, -2));
    assertEquals(first.plus(2), second);
    assertEquals(first.plus(second), new Interval(6, 8));
    assertEquals(second.minus(2), first);
    assertEquals(first.minus(second), new Interval(-3, -1));
    assertEquals(first.multipliedBy(Sign.Negative()), first.negated());
    assertEquals(first.multipliedBy(0.5), new Interval(1, 1.5));
    assertEquals(first.multipliedBy(second), new Interval(8, 15));
    assertEquals(second.dividedBy(2), new Interval(2, 2.5));
    Interval quotient = second.dividedBy(first);
    assertEquals(quotient.lowerBound(), 4.0 / 3.0, 1e-12);
    assertEquals(quotient.upperBound(), 2.5, 1e-12);
  }
}
