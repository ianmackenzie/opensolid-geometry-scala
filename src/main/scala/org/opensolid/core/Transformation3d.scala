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

abstract class Transformation3d {
  def apply(point: Point3d): Point3d

  def apply(vector: Vector3d): Vector3d

  def andThen(that: Transformation3d): Transformation3d = CompoundTransformation3d(this, that)

  def compose(that: Transformation3d): Transformation3d = CompoundTransformation3d(that, this)
}

object Transformation3d {
  implicit class TransformationFunction3d[T <: Transformable3d[T]](
    transformation: Transformation3d
  ) extends AbstractFunction1[T, T] {

    def apply(transformable: T): T = transformable.transformedBy(transformation)
  }

  implicit class VectorTransformationFunction3d[T <: VectorTransformable3d[T]](
    transformation: Transformation3d
  ) extends AbstractFunction1[T, T] {

    def apply(transformable: T): T = transformable.transformedBy(transformation)
  }
}
