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

final case class DirectionBox2d(vectorBox: VectorBox2d) {
  def this(x: Interval, y: Interval) = this(VectorBox2d(x, y))

  def this(components: (Interval, Interval)) = this(components.first, components.second)

  def x: Interval = vectorBox.x

  def y: Interval = vectorBox.y

  def components: (Interval, Interval) = vectorBox.components

  def component(index: Int): Interval = vectorBox.component(index)

  def unary_- : DirectionBox2d = DirectionBox2d(-vectorBox)

  def *(sign: Sign): DirectionBox2d = sign match {
    case Sign.Positive => this
    case Sign.Negative => -this
    case _ => DirectionBox2d.Empty
  }

  def *(value: Double): VectorBox2d = vectorBox * value

  def *(interval: Interval): VectorBox2d = vectorBox * interval

  def /(value: Double): VectorBox2d = vectorBox / value

  def /(interval: Interval): VectorBox2d = vectorBox / interval

  def dot(vector: Vector2d): Interval = vectorBox.dot(vector)

  def dot(vectorBox: VectorBox2d): Interval = this.vectorBox.dot(vectorBox)

  def dot(direction: Direction2d): Interval = vectorBox.dot(direction.vector)

  def dot(that: DirectionBox2d): Interval = this.vectorBox.dot(that.vectorBox)
}

object DirectionBox2d {
  def apply(x: Interval, y: Interval): DirectionBox2d = new DirectionBox2d(x, y)

  def apply(components: (Interval, Interval)): DirectionBox2d = new DirectionBox2d(components)

  val Empty = DirectionBox2d(Interval.Empty, Interval.Empty)
}
