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

final case class Direction2d(x: Double, y: Double) extends VectorTransformable2d[Direction2d] {
  def components: Array[Double] = Array(x, y)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Direction2d")
  }

  def vector: Vector2d = Vector2d(x, y)

  def unary_- : Direction2d = Direction2d(-x, -y)

  def *(sign: Sign): Direction2d = Direction2d(x * sign, y * sign)

  def *(value: Double): Vector2d = Vector2d(x * value, y * value)

  def *(interval: Interval): VectorBox2d = VectorBox2d(x * interval, y * interval)

  def /(value: Double): Vector2d = Vector2d(x / value, y / value)

  def /(interval: Interval): VectorBox2d = VectorBox2d(x / interval, y / interval)

  def transformedBy(transformation: Transformation2d): Direction2d = transformation(this)

  def dot(vector: Vector2d): Double = x * vector.x + y * vector.y

  def dot(that: Direction2d): Double = this.x * that.x + this.y * that.y

  def dot(vectorBox: VectorBox2d): Interval = x * vectorBox.x + y * vectorBox.y

  def dot(directionBox: DirectionBox2d): Interval = x * directionBox.x + y * directionBox.y

  def normalDirection: Direction2d = Direction2d(-y, x)
}

object Direction2d {
  def fromComponents[T <% Double](components: Seq[T]): Direction2d = components match {
    case Seq(x, y) => Direction2d(x, y)
    case _ => throw new IllegalArgumentException("Direction2d requires 2 components")
  }

  def polar(angle: Double): Direction2d = Direction2d(math.cos(angle), math.sin(angle))

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
