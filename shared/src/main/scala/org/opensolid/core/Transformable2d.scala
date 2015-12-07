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

trait Transformable2d[T] {
  def transformedBy(transformation: Transformation2d): T

  def translatedBy(vector: Vector2d): T = transformedBy(Translation2d(vector))

  def translatedBy(x: Double, y: Double): T = transformedBy(Translation2d(x, y))

  def translatedAlong(axis: Axis2d, distance: Double): T =
    transformedBy(Translation2d(axis, distance))

  def rotatedAbout(point: Point2d, angle: Double): T = transformedBy(Rotation2d(point, angle))

  def relativeTo(frame: Frame2d): T = transformedBy(Localization2d(frame))

  def placedIn(frame: Frame2d): T = transformedBy(Globalization2d(frame))

  def mirroredAbout(point: Point2d, direction: Direction2d): T =
    transformedBy(Mirror2d(point, direction))

  def mirroredAbout(axis: Axis2d): T = transformedBy(Mirror2d(axis))
}
