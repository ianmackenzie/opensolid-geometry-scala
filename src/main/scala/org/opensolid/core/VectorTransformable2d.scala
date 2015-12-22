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

trait VectorTransformable2d[T] {
  def transformedBy(transformation: Transformation2d): T

  def rotatedBy(angle: Double): T = transformedBy(Rotation2d(Point2d.Origin, angle))

  def relativeTo(frame: Frame2d): T = transformedBy(Localization2d(frame))

  def placedIn(frame: Frame2d): T = transformedBy(Globalization2d(frame))

  def mirroredAlong(direction: Direction2d): T = transformedBy(Mirror2d(Point2d.Origin, direction))

  def mirroredAbout(axis: Axis2d): T = transformedBy(Mirror2d(axis))
}
