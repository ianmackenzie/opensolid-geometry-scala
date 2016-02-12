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

/** Base trait for objects such as points, triangles or parametric curves that can be transformed in
  * 2D (rotated, translated etc.). All functions return a transformed copy of the original object.
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

  /** Returns this object translated by the given vector. */
  def translatedBy(vector: Vector2d): T =
    transformedBy(Translation2d(vector))

  /** Returns this object translated by the given x and y components.*/
  def translatedBy(x: Double, y: Double): T =
    transformedBy(Translation2d(x, y))

  /** Returns this object translated along the given axis by the given distance.
    *
    * Equivalent to `translatedBy(distance * axis.direction)`.
    */
  def translatedAlong(axis: Axis2d, distance: Double): T =
    transformedBy(Translation2d.along(axis, distance))

  /** Returns this object rotated about the given point by the given angle (in radians). */
  def rotatedAbout(point: Point2d, angle: Double): T =
    transformedBy(Rotation2d(point, angle))

  /** Global-to-local transformation: returns this object as expressed relative to the given
    * reference frame, assuming it is currently defined in global coordinates.
    *
    * For instance, the expression `curve.relativeTo(frame)` assumes that both `curve` and `frame`
    * are themselves defined in global coordinates, and returns a copy of `curve` expressed in local
    * coordinates relative to `frame`.
    */
  def relativeTo(frame: Frame2d): T =
    transformedBy(Localization2d(frame))

  /** Local-to-global transformation: returns this object in global coordinates, assuming it is
    * currently defined relative to the given reference frame.
    *
    * For example:
    * {{{
    * val frame = Frame2d(Point2d(3, 4), Direction2d.X, Direction2d.Y)
    * val point = Point2d(1, 1).placedIn(frame) // Point2d(4, 5)
    * }}}
    * Slightly more complex: take the point (1, 0) (a distance of 1 along the X axis) and place it
    * in a rotated frame. The result is a point a distance of 1 along the rotated frame's X axis, in
    * global coordinates:
    * {{{
    * val frame = Frame2d.Global.rotatedAbout(Point2d.Origin, 45.degrees)
    * val point = Point2d(1, 0).placedIn(frame) // Point2d(0.707, 0.707)
    * }}}
    */
  def placedIn(frame: Frame2d): T =
    transformedBy(Globalization2d(frame))

  /** Returns this object mirrored about the given axis.
    *
    * Note that care must be taken since several methods on various types of objects are defined
    * to behave in a right-handed sense, and will continue to do so even for mirrored objects with
    * potentially surprising results.

    * For instance, the normal direction of a 2D line segment is defined to point to the left of the
    * line segment. Consider a closed polygon defined by a list of line segments where the normal
    * direction of each line segment points outwards. If each line segment in that list is mirrored
    * about some axis, the resulting closed polygon will have the correct mirrored shape but all
    * normal directions will now point inwards. This may mean that in addition to mirroring, all of
    * the mirrored line segments should be flipped back-to-front to restore the outwards-pointing
    * normals property (and perhaps the list itself should be reversed so that all line segments
    * continue to touch head-to-tail).
    */
  def mirroredAbout(axis: Axis2d): T =
    transformedBy(Mirror2d(axis))
}
