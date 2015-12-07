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

public class Vector2dTests {
  @Test
  public void run() {
    Vector2d vector = new Vector2d(1.0, 2.0);
    Vector2d zeroVector = Vector2d.getZero();
    Vector2d polar = Vector2d.polar(2.0, Math.PI / 6.0);
    double x = vector.getX();
    double y = vector.getY();
    double alsoX = vector.component(0);
    double squaredLength = vector.getSquaredLength();
    double length = vector.getLength();
    boolean isZero = vector.isZero(1e-3);
    boolean isNotZero = vector.isNotZero(1e-3);
    Vector2d transformed =
        vector.transformedBy(new Rotation2d(Point2d.getOrigin(), Math.toRadians(45)));
    Vector2d rotated = (Vector2d) vector.rotatedBy(Math.PI / 4.0);
    Vector2d relativeTo = (Vector2d) vector.relativeTo(Frame2d.getGlobal());
    Vector2d placedIn = (Vector2d) vector.placedIn(Frame2d.getGlobal());
    Vector2d mirroredAlong = (Vector2d) vector.mirroredAlong(Direction2d.getX());
    Vector2d mirroredAbout = (Vector2d) vector.mirroredAbout(Axis2d.getY());
    Vector2d projected = vector.projectedOnto(Axis2d.getX());
    Vector3d placed = vector.placedOnto(Plane3d.getXY());
    Vector2d normalized = vector.getNormalized();
    Direction2d direction = vector.getDirection();
    Direction2d normalDirection = vector.getNormalDirection();
    Vector2d negated = vector.negated();
    Vector2d plusVector = vector.plus(vector);
    VectorBoundingBox2d plusVectorBoundingBox = vector.plus(VectorBoundingBox2d.getEmpty());
    Vector2d minusVector = vector.minus(vector);
    VectorBoundingBox2d minusVectorBoundingBox = vector.minus(VectorBoundingBox2d.getEmpty());
    Vector2d timesSign = vector.times(Sign.getNegative());
    Vector2d timesDouble = vector.times(2.0);
    VectorBoundingBox2d timesInterval = vector.times(Interval.getUnit());
    Vector2d dividedByDouble = vector.dividedBy(2.0);
    VectorBoundingBox2d dividedByInterval = vector.dividedBy(Interval.getUnit());
    double dotVector = vector.dot(vector);
    double dotDirection = vector.dot(Direction2d.getY());
    Interval dotVectorBoundingBox = vector.dot(VectorBoundingBox2d.getEmpty());
    Interval dotDirectionBoundingBox = vector.dot(DirectionBoundingBox2d.getEmpty());
  }
}