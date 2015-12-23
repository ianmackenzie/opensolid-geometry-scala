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

import scala.util.Random

final case class VectorBox2d(x: Interval, y: Interval) {
  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for VectorBox2d")
  }

  def isEmpty: Boolean = x.isEmpty || y.isEmpty

  def isWhole: Boolean = x.isWhole && y.isWhole

  def isSingleton: Boolean = x.isSingleton && y.isSingleton

  def center: Vector2d = Vector2d(x.median, y.median)

  def interpolated(u: Double, v: Double): Vector2d = Vector2d(x.interpolated(u), y.interpolated(v))

  def randomVector: Vector2d = randomVector(Random)

  def randomVector(generator: Random): Vector2d =
    interpolated(generator.nextDouble, generator.nextDouble)

  def hull(vector: Vector2d): VectorBox2d = VectorBox2d(x.hull(vector.x), y.hull(vector.y))

  def hull(that: VectorBox2d): VectorBox2d = VectorBox2d(this.x.hull(that.x), this.y.hull(that.y))

  def intersection(that: VectorBox2d): VectorBox2d = {
    val x = this.x.intersection(that.x)
    val y = this.y.intersection(that.y)
    if (x.isEmpty || y.isEmpty) VectorBox2d.Empty else VectorBox2d(x, y)
  }

  def overlaps(that: VectorBox2d): Boolean =
    this.x.overlaps(that.x) && this.y.overlaps(that.y)

  def overlaps(that: VectorBox2d, tolerance: Double): Boolean =
    this.x.overlaps(that.x, tolerance) && this.y.overlaps(that.y, tolerance)

  def contains(vector: Vector2d): Boolean = x.contains(vector.x) && y.contains(vector.y)

  def contains(vector: Vector2d, tolerance: Double): Boolean =
    x.contains(vector.x, tolerance) && y.contains(vector.y, tolerance)

  def contains(that: VectorBox2d): Boolean =
    this.x.contains(that.x) && this.y.contains(that.y)

  def contains(that: VectorBox2d, tolerance: Double): Boolean =
    this.x.contains(that.x, tolerance) && this.y.contains(that.y, tolerance)

  def squaredLength: Interval = x * x + y * y

  def length: Interval = Interval.sqrt(squaredLength)

  def normalized: VectorBox2d = directionBox.vectorBox

  def directionBox: DirectionBox2d = {
    if (this == VectorBox2d.Zero) {
      DirectionBox2d.Empty
    } else {
      val length = this.length
      DirectionBox2d(x / length, y / length)
    }
  }

  def unary_- : VectorBox2d = VectorBox2d(-x, -y)

  def +(vector: Vector2d): VectorBox2d = VectorBox2d(x + vector.x, y + vector.y)

  def +(that: VectorBox2d): VectorBox2d = VectorBox2d(this.x + that.x, this.y + that.y)

  def -(vector: Vector2d): VectorBox2d = VectorBox2d(x - vector.x, y - vector.y)

  def -(that: VectorBox2d): VectorBox2d = VectorBox2d(this.x - that.x, this.y - that.y)

  def *(sign: Sign): VectorBox2d = sign match {
    case Sign.Positive => this
    case Sign.Negative => -this
    case _ => VectorBox2d.Empty
  }

  def *(value: Double): VectorBox2d = VectorBox2d(x * value, y * value)

  def *(interval: Interval): VectorBox2d = VectorBox2d(x * interval, y * interval)

  def /(value: Double): VectorBox2d = VectorBox2d(x / value, y / value)

  def /(interval: Interval): VectorBox2d = VectorBox2d(x / interval, y / interval)

  def dot(vector: Vector2d): Interval = x * vector.x + y * vector.y

  def dot(direction: Direction2d): Interval = dot(direction.vector)

  def dot(that: VectorBox2d): Interval = this.x * that.x + this.y * that.y

  def dot(directionBox: DirectionBox2d): Interval = dot(directionBox.vectorBox)
}

object VectorBox2d {
  val Empty: VectorBox2d = VectorBox2d(Interval.Empty, Interval.Empty)

  val Whole: VectorBox2d = VectorBox2d(Interval.Whole, Interval.Whole)

  val Zero: VectorBox2d = VectorBox2d(Interval.Zero, Interval.Zero)
}
