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

  def rotatedAbout(point: Point2d, angle: Double): T = transformedBy(Rotation2d(point, angle))

  def relativeTo(frame: Frame2d): T = transformedBy(Localization2d(frame))
}
