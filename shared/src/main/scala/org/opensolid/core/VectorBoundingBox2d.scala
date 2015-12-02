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

final case class VectorBoundingBox2d(x: Interval, y: Interval) {
  def components: Array[Interval] = Array(x, y)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case _ =>
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds for VectorBoundingBox2d")
  }

  def isEmpty: Boolean = x.isEmpty || y.isEmpty

  def isWhole: Boolean = x.isWhole && y.isWhole

  def isSingleton: Boolean = x.isSingleton && y.isSingleton

  def center: Vector2d = Vector2d(x.median, y.median)

  def interpolated(u: Double, v: Double): Vector2d = Vector2d(x.interpolated(u), y.interpolated(v))

  def randomVector: Vector2d = randomVector(Random)

  def randomVector(generator: Random): Vector2d =
    interpolated(generator.nextDouble, generator.nextDouble)

  def hull(vector: Vector2d): VectorBoundingBox2d =
    VectorBoundingBox2d(x.hull(vector.x), y.hull(vector.y))

  def hull(that: VectorBoundingBox2d): VectorBoundingBox2d =
    VectorBoundingBox2d(this.x.hull(that.x), this.y.hull(that.y))

  def intersection(that: VectorBoundingBox2d): VectorBoundingBox2d = {
    val x = this.x.intersection(that.x)
    val y = this.y.intersection(that.y)
    if (x.isEmpty || y.isEmpty) VectorBoundingBox2d.Empty else VectorBoundingBox2d(x, y)
  }

  def overlaps(that: VectorBoundingBox2d): Boolean =
    this.x.overlaps(that.x) && this.y.overlaps(that.y)

  def overlaps(that: VectorBoundingBox2d, tolerance: Double): Boolean =
    this.x.overlaps(that.x, tolerance) && this.y.overlaps(that.y, tolerance)

  def contains(vector: Vector2d): Boolean = x.contains(vector.x) && y.contains(vector.y)

  def contains(vector: Vector2d, tolerance: Double): Boolean =
    x.contains(vector.x, tolerance) && y.contains(vector.y, tolerance)

  def contains(that: VectorBoundingBox2d): Boolean =
    this.x.contains(that.x) && this.y.contains(that.y)

  def contains(that: VectorBoundingBox2d, tolerance: Double): Boolean =
    this.x.contains(that.x, tolerance) && this.y.contains(that.y, tolerance)

  def squaredLength: Interval = x * x + y * y

  def length: Interval = Interval.sqrt(squaredLength)

  def normalized: VectorBoundingBox2d = directionBoundingBox.vectorBoundingBox

  def directionBoundingBox: DirectionBoundingBox2d = {
    if (this == VectorBoundingBox2d.Zero) {
      DirectionBoundingBox2d.Empty
    } else {
      val length = this.length
      DirectionBoundingBox2d(x / length, y / length)
    }
  }

  def unary_- : VectorBoundingBox2d = VectorBoundingBox2d(-x, -y)

  def +(vector: Vector2d): VectorBoundingBox2d =
    VectorBoundingBox2d(x + vector.x, y + vector.y)

  def +(that: VectorBoundingBox2d): VectorBoundingBox2d =
    VectorBoundingBox2d(this.x + that.x, this.y + that.y)

  def -(vector: Vector2d): VectorBoundingBox2d =
    VectorBoundingBox2d(x - vector.x, y - vector.y)

  def -(that: VectorBoundingBox2d): VectorBoundingBox2d =
    VectorBoundingBox2d(this.x - that.x, this.y - that.y)

  def *(sign: Sign): VectorBoundingBox2d = sign match {
    case Sign.Positive => this
    case Sign.Negative => -this
    case _ => VectorBoundingBox2d.Empty
  }

  def *(value: Double): VectorBoundingBox2d = VectorBoundingBox2d(x * value, y * value)

  def *(interval: Interval): VectorBoundingBox2d = VectorBoundingBox2d(x * interval, y * interval)

  def /(value: Double): VectorBoundingBox2d = VectorBoundingBox2d(x / value, y / value)

  def /(interval: Interval): VectorBoundingBox2d = VectorBoundingBox2d(x / interval, y / interval)

  def dot(vector: Vector2d): Interval = x * vector.x + y * vector.y

  def dot(direction: Direction2d): Interval = dot(direction.vector)

  def dot(that: VectorBoundingBox2d): Interval = this.x * that.x + this.y * that.y

  def dot(directionBoundingBox: DirectionBoundingBox2d): Interval =
    dot(directionBoundingBox.vectorBoundingBox)
}

object VectorBoundingBox2d {
  def fromComponents(components: Seq[Interval]): VectorBoundingBox2d = components match {
    case Seq(x, y) => VectorBoundingBox2d(x, y)
    case _ => throw new IllegalArgumentException("VectorBoundingBox2d requires 2 components")
  }

  val Empty: VectorBoundingBox2d = VectorBoundingBox2d(Interval.Empty, Interval.Empty)

  val Whole: VectorBoundingBox2d = VectorBoundingBox2d(Interval.Whole, Interval.Whole)

  val Zero: VectorBoundingBox2d = VectorBoundingBox2d(Interval.Zero, Interval.Zero)
}
