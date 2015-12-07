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

abstract class Transformable3d[T] {
  def transformedBy(transformation: Transformation3d): T

  def translatedBy(vector: Vector3d): T = transformedBy(Translation3d(vector))

  def translatedBy(x: Double, y: Double, z: Double): T = transformedBy(Translation3d(x, y, z))

  def translatedAlong(axis: Axis3d, distance: Double): T =
    transformedBy(Translation3d(axis, distance))

  def rotatedAbout(axis: Axis3d, angle: Double): T = transformedBy(Rotation3d(axis, angle))

  def rotatedAboutX(point: Point3d, angle: Double): T =
    transformedBy(Rotation3d.aboutX(point, angle))

  def rotatedAboutY(point: Point3d, angle: Double): T =
    transformedBy(Rotation3d.aboutY(point, angle))

  def rotatedAboutZ(point: Point3d, angle: Double): T =
    transformedBy(Rotation3d.aboutZ(point, angle))

  def relativeTo(frame: Frame3d): T = transformedBy(Localization3d(frame))

  def placedIn(frame: Frame3d): T = transformedBy(Globalization3d(frame))

  def mirroredAbout(point: Point3d, direction: Direction3d): T =
    transformedBy(Mirror3d(point, direction))

  def mirroredAbout(plane: Plane3d): T = transformedBy(Mirror3d(plane))
}
