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
import scala.Tuple2;
import scala.util.Random;

public class IntervalTests {
  @Test
  public void run() {
    Interval interval = new Interval(1.0, 3.0);
    Interval other = new Interval(2.0, 4.0);
    Interval singleton = new Interval(5.0);
    Interval empty = Interval.getEmpty();
    Interval whole = Interval.getWhole();
    Interval unit = Interval.getUnit();
    Interval zero = Interval.getZero();

    double lowerBound = interval.lowerBound();
    double upperBound = interval.upperBound();
    boolean equal = interval.equals(other);
    int hashCode = interval.hashCode();
    String string = interval.toString();

    Interval bounds = interval.bounds();
    boolean isEmpty = interval.isEmpty();
    boolean isWhole = interval.isWhole();
    double width = interval.width();
    double interpolated = interval.interpolated(0.5);
    double median = interval.median();
    double randomValue = interval.randomValue();
    Random generator = new Random();
    double randomValueFromGenerator = interval.randomValue(generator);

    boolean isSingleton = interval.isSingleton();

    Tuple2<Interval, Interval> bisected = interval.bisected();
    Interval lowerHalf = bisected._1();
    Interval upperHalf = bisected._2();

    Interval hullValue = interval.hull(3.0);
    Interval hullInterval = interval.hull(other);
    Interval intersection = interval.intersection(other);

    boolean containsValue = interval.contains(1.0);
    boolean tolerantContainsValue = interval.contains(1.0, 1e-3);
    boolean containsInterval = interval.contains(other);
    boolean tolerantContainsInterval = interval.contains(other, 1e-3);

    boolean overlaps = interval.overlaps(other);
    boolean tolerantOverlaps = interval.overlaps(other, 1e-3);

    Interval negated = interval.negated();
    Interval plusDouble = interval.plus(1.0);
    Interval plusInterval = interval.plus(other);
    Interval minusDouble = interval.minus(1.0);
    Interval minusInterval = interval.minus(other);
    Interval timesSign = interval.times(Sign.Negative());
    Interval timesDouble = interval.times(2.0);
    Interval timesInterval = interval.times(other);
    Interval dividedByDouble = interval.dividedBy(2.0);
    Interval dividedByInterval = interval.dividedBy(other);

    Interval abs = interval.abs();
    Interval squared = interval.squared();

    Interval sqrt = Interval.sqrt(interval);
    Interval sin = Interval.sin(interval);
    Interval cos = Interval.cos(interval);
    Interval tan = Interval.tan(interval);
    Interval asin = Interval.asin(interval);
    Interval acos = Interval.acos(interval);
    Interval atan = Interval.atan(interval);
    Interval atan2 = Interval.atan2(interval, other);
    Interval exp = Interval.exp(interval);
    Interval log = Interval.log(interval);

    double ulp = Interval.ulp(interval);
  }
}
