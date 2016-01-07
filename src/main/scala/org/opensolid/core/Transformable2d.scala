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

/**
  *
  */
trait Transformable2d[T] {
  /** Returns this object transformed by an arbitrary transformation.
    *
    * All other transformation functions simply call this function with the appropriate
    * `Transformation2d` object. For instance, `point.translatedBy(x, y)` is equivalent to (and
    * implemented as) `point.transformedBy(Translation2d(x, y))`.
    *
    * In most cases it is easier to use the other transformation functions directly. However,
    * using `transformedBy` is more efficient if the same transformation is applied to many objects,
    * especially for transformations such as rotations which involve non-trivial initialization. For
    * instance, the following are equivalent but the second version is faster as it only creates a
    * single rotation transformation object instead of implicitly creating one for each point in the
    * list:
    * {{{
    * val points: List[Point2d] = ???
    *
    * val rotated1 = points.map(_.rotatedAbout(Point2d.Origin, 45.degrees))
    *
    * val rotation = Rotation2d(Point2d.Origin, 45.degrees)
    * val rotated2 = points.map(_.transformedBy(rotation))
    * }}}
    * In this case, however, it would be simpler to exploit the fact that Transformation2d objects
    * can be used as functions:
    * {{{
    * val rotated3 = points.map(rotation)
    * }}}
    */
  def transformedBy(transformation: Transformation2d): T

  def translatedBy(vector: Vector2d): T = transformedBy(Translation2d(vector))

  def translatedBy(x: Double, y: Double): T = transformedBy(Translation2d(x, y))

  def translatedAlong(axis: Axis2d, distance: Double): T =
    transformedBy(Translation2d(axis, distance))

  def rotatedAbout(point: Point2d, angle: Double): T = transformedBy(Rotation2d(point, angle))

  def relativeTo(frame: Frame2d): T = transformedBy(Localization2d(frame))

  def placedIn(frame: Frame2d): T = transformedBy(Globalization2d(frame))

  def mirroredAbout(axis: Axis2d): T = transformedBy(Mirror2d(axis))
}
