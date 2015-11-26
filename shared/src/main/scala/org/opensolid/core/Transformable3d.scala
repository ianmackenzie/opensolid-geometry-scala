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

trait Transformable3d[T] {
  def transformedBy(transformation: Transformation3d): T

  def translatedBy(vector: Vector3d): T = transformedBy(Translation3d(vector))

  def rotatedAbout(axis: Axis3d, angle: Double): T = transformedBy(Rotation3d(axis, angle))

  def relativeTo(frame: Frame3d): T = transformedBy(Localization3d(frame))

  def placedIn(frame: Frame3d): T = transformedBy(Globalization3d(frame))
}
