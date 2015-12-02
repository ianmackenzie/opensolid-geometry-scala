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

final case class DirectionBoundingBox2d(vectorBoundingBox: VectorBoundingBox2d) {
  def x: Interval = vectorBoundingBox.x

  def y: Interval = vectorBoundingBox.y

  def components: Array[Interval] = vectorBoundingBox.components

  def component(index: Int): Interval = vectorBoundingBox.component(index)

  def unary_- : DirectionBoundingBox2d = DirectionBoundingBox2d(-vectorBoundingBox)

  def *(sign: Sign): DirectionBoundingBox2d = DirectionBoundingBox2d(vectorBoundingBox * sign)

  def *(value: Double): VectorBoundingBox2d = vectorBoundingBox * value

  def *(interval: Interval): VectorBoundingBox2d = vectorBoundingBox * interval

  def /(value: Double): VectorBoundingBox2d = vectorBoundingBox / value

  def /(interval: Interval): VectorBoundingBox2d = vectorBoundingBox / interval

  def dot(vector: Vector2d): Interval = vectorBoundingBox.dot(vector)

  def dot(vectorBoundingBox: VectorBoundingBox2d): Interval =
    this.vectorBoundingBox.dot(vectorBoundingBox)

  def dot(direction: Direction2d): Interval = vectorBoundingBox.dot(direction.vector)

  def dot(that: DirectionBoundingBox2d): Interval =
    this.vectorBoundingBox.dot(that.vectorBoundingBox)
}

object DirectionBoundingBox2d {
  def apply(x: Interval, y: Interval): DirectionBoundingBox2d =
    DirectionBoundingBox2d(VectorBoundingBox2d(x, y))

  def apply(direction: Direction2d): DirectionBoundingBox2d =
    DirectionBoundingBox2d(Interval(direction.x), Interval(direction.y))

  def fromComponents(components: Seq[Interval]): DirectionBoundingBox2d = components match {
    case Seq(x, y) => DirectionBoundingBox2d(x, y)
    case _ => throw new IllegalArgumentException("DirectionBoundingBox2d requires 2 components")
  }

  val Empty = DirectionBoundingBox2d(Interval.Empty, Interval.Empty)
}
