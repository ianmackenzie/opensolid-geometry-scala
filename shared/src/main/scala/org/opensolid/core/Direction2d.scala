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

import scala.annotation.tailrec
import scala.math
import scala.util.Random

final class Direction2d(val vector: Vector2d) extends VectorTransformable2d[Direction2d] {
  override def equals(other: Any): Boolean = other match {
    case that: Direction2d => this.vector == that.vector
    case _ => false
  }

  override def hashCode: Int = vector.hashCode

  def x: Double = vector.x

  def y: Double = vector.y

  def components: Array[Double] = vector.components

  def component(index: Int): Double = vector.component(index)

  def unary_- : Direction2d = Direction2d(-vector)

  def *(sign: Sign): Direction2d = Direction2d(vector * sign)

  def *(value: Double): Vector2d = vector * value

  def *(interval: Interval): VectorBox2d = vector * interval

  def /(value: Double): Vector2d = vector / value

  def /(interval: Interval): VectorBox2d = vector / interval

  def transformedBy(transformation: Transformation2d): Direction2d = transformation(this)

  def dot(vector: Vector2d): Double = this.vector.dot(vector)

  def dot(that: Direction2d): Double = this.vector.dot(that.vector)

  def dot(vectorBox: VectorBox2d): Interval = vector.dot(vectorBox)

  def dot(directionBox: DirectionBox2d): Interval = vector.dot(directionBox.vectorBox)

  def normalDirection: Direction2d = Direction2d(-y, x)

  def angleTo(that: Direction2d): Double =
    math.atan2(this.x * that.y - this.y * that.x, this.x * that.x + this.y * that.y)
}

object Direction2d {
  def apply(vector: Vector2d): Direction2d = new Direction2d(vector)

  def apply(x: Double, y: Double): Direction2d = Direction2d(Vector2d(x, y))

  def unapply(direction: Direction2d): Option[(Double, Double)] =
    Some((direction.x, direction.y))

  def fromComponents[T <% Double](components: Seq[T]): Direction2d = components match {
    case Seq(x, y) => Direction2d(x, y)
    case _ => throw new IllegalArgumentException("Direction2d requires 2 components")
  }

  def fromAngle(angle: Double): Direction2d = Direction2d(math.cos(angle), math.sin(angle))

  def random: Direction2d = random(Random)

  def random(generator: Random): Direction2d = {
    @tailrec
    def generate: Direction2d = {
      val x = -1.0 + 2.0 * generator.nextDouble
      val y = -1.0 + 2.0 * generator.nextDouble
      val squaredNorm = x * x + y * y
      if (squaredNorm >= 0.25 && squaredNorm <= 1.0) {
        val norm = math.sqrt(squaredNorm)
        Direction2d(x / norm, y / norm)
      } else {
        generate
      }
    }
    generate
  }

  val None: Direction2d = Direction2d(0.0, 0.0)

  val X: Direction2d = Direction2d(1.0, 0.0)

  val Y: Direction2d = Direction2d(0.0, 1.0)
}
