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
  def this(vector: Vector2d) =
    this(vector.x, vector.y)

  def components: (Double, Double) =
    (x, y)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Direction2d")
  }

  def vector: Vector2d =
    Vector2d(x, y)

  def unary_- : Direction2d =
    Direction2d(-x, -y)

  def negated: Direction2d =
    -this

  def *(value: Double): Vector2d =
    Vector2d(x * value, y * value)

  def times(value: Double): Vector2d =
    this * value

  def transformedBy(transformation: Transformation2d): Direction2d =
    transformation(this)

  def placedOnto(plane: Plane3d): Direction3d =
    Direction3d(vector.placedOnto(plane))

  def normalDirection: Direction2d =
    Direction2d(-y, x)

  def angleTo(that: Direction2d): Double =
    math.atan2(x * that.y - y * that.x, x * that.x + y * that.y)
}

object Direction2d {
  def apply(vector: Vector2d): Direction2d =
    Direction2d(vector.x, vector.y)

  def fromComponents(components: (Double, Double)): Direction2d = components match {
    case (x, y) => Direction2d(x, y)
  }

  def polar(angle: Double): Direction2d =
    Direction2d(math.cos(angle), math.sin(angle))

  def random: Direction2d =
    random(Random)

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

  val X: Direction2d = Direction2d(1.0, 0.0)

  val Y: Direction2d = Direction2d(0.0, 1.0)
}
