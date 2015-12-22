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

import scala.runtime.AbstractFunction1

abstract class Transformation2d {
  def apply(length: Double): Double

  def apply(handedness: Handedness): Handedness

  def apply(point: Point2d): Point2d

  def apply(vector: Vector2d): Vector2d

  def apply(direction: Direction2d): Direction2d

  def andThen(that: Transformation2d): Transformation2d = CompoundTransformation2d(this, that)

  def compose(that: Transformation2d): Transformation2d = CompoundTransformation2d(that, this)
}

object Transformation2d {
  implicit class TransformationFunction2d[T <: Transformable2d[T]](
    transformation: Transformation2d
  ) extends AbstractFunction1[T, T] {

    def apply(transformable: T): T = transformable.transformedBy(transformation)
  }

  implicit class VectorTransformationFunction2d[T <: VectorTransformable2d[T]](
    transformation: Transformation2d
  ) extends AbstractFunction1[T, T] {

    def apply(transformable: T): T = transformable.transformedBy(transformation)
  }
}
