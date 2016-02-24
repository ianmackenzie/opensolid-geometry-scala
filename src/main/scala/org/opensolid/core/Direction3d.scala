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

final case class Direction3d(x: Double, y: Double, z: Double)
  extends VectorTransformable3d[Direction3d] {

  def this(vector: Vector3d) =
    this(vector.x, vector.y, vector.z)

  def components: (Double, Double, Double) =
    (x, y, z)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Direction3d")
  }

  def vector: Vector3d =
    Vector3d(x, y, z)

  def unary_- : Direction3d =
    Direction3d(-x, -y, -z)

  def negated: Direction3d =
    -this

  def *(value: Double): Vector3d =
    Vector3d(x * value, y * value, z * value)

  def times(value: Double): Vector3d =
    this * value

  def transformedBy(transformation: Transformation3d): Direction3d =
    transformation(this)

  def projectedOnto(plane: Plane3d): Direction3d =
    vector.projectedOnto(plane).direction

  def projectedInto(plane: Plane3d): Direction2d =
    vector.projectedInto(plane).direction

  def normalDirection: Direction3d =
    vector.normalDirection

  def angleTo(that: Direction3d): Double =
    math.acos(x * that.x + y * that.y + z * that.z)

  def componentIn(that: Direction3d): Double =
    this.x * that.x + this.y * that.y + this.z * that.z
}

object Direction3d {
  def apply(vector: Vector3d): Direction3d =
    Direction3d(vector.x, vector.y, vector.z)

  def fromComponents(components: (Double, Double, Double)): Direction3d = components match {
    case (x, y, z) => Direction3d(x, y, z)
  }

  def spherical(azimuth: Double, elevation: Double): Direction3d = {
    val cosElevation = math.cos(elevation)
    val sinElevation = math.sin(elevation)
    val cosAzimuth = math.cos(azimuth)
    val sinAzimuth = math.sin(azimuth)
    Direction3d(cosElevation * cosAzimuth, cosElevation * sinAzimuth, sinElevation)
  }

  def random: Direction3d =
    random(Random)

  def random(generator: Random): Direction3d = {
    @tailrec
    def generate: Direction3d = {
      val x = -1.0 + 2.0 * generator.nextDouble
      val y = -1.0 + 2.0 * generator.nextDouble
      val z = -1.0 + 2.0 * generator.nextDouble
      val squaredNorm = x * x + y * y + z * z
      if (squaredNorm >= 0.25 && squaredNorm <= 1.0) {
        val norm = math.sqrt(squaredNorm)
        Direction3d(x / norm, y / norm, z / norm)
      } else {
        generate
      }
    }
    generate
  }

  val X: Direction3d = Direction3d(1.0, 0.0, 0.0)

  val Y: Direction3d = Direction3d(0.0, 1.0, 0.0)

  val Z: Direction3d = Direction3d(0.0, 0.0, 1.0)
}
