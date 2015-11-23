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

final class DirectionBox2d(val vectorBox: VectorBox2d) {
  override def equals(other: Any): Boolean = other match {
    case that: DirectionBox2d => this.vectorBox == that.vectorBox
    case _ => false
  }

  override def hashCode: Int = vectorBox.hashCode

  def x: Interval = vectorBox.x

  def y: Interval = vectorBox.y

  def components: Array[Interval] = vectorBox.components

  def component(index: Int): Interval = vectorBox.component(index)

  def unary_- : DirectionBox2d = DirectionBox2d(-vectorBox)

  def *(sign: Sign): DirectionBox2d = DirectionBox2d(vectorBox * sign)

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
  def apply(vectorBox: VectorBox2d): DirectionBox2d = new DirectionBox2d(vectorBox)

  def apply(x: Interval, y: Interval): DirectionBox2d = DirectionBox2d(VectorBox2d(x, y))

  def unapply(directionBox: DirectionBox2d): Option[(Interval, Interval)] =
    Some((directionBox.x, directionBox.y))

  def apply(direction: Direction2d): DirectionBox2d =
    DirectionBox2d(Interval(direction.x), Interval(direction.y))

  def fromComponents(components: Seq[Interval]): DirectionBox2d = components match {
    case Seq(x, y) => DirectionBox2d(x, y)
    case _ => throw new IllegalArgumentException("DirectionBox2d requires 2 components")
  }

  val Empty = DirectionBox2d(Interval.Empty, Interval.Empty)
}
