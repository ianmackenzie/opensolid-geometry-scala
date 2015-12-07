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

final case class Direction3d(vector: Vector3d) extends VectorTransformable3d[Direction3d] {
  def x: Double = vector.x

  def y: Double = vector.y

  def z: Double = vector.z

  def components: Array[Double] = vector.components

  def component(index: Int): Double = vector.component(index)

  def unary_- : Direction3d = Direction3d(-vector)

  def *(sign: Sign): Direction3d = sign match {
    case Sign.Positive => this
    case Sign.Negative => -this
    case _ => Direction3d.None
  }

  def *(value: Double): Vector3d = vector * value

  def *(interval: Interval): VectorBoundingBox3d = vector * interval

  def /(value: Double): Vector3d = vector / value

  def /(interval: Interval): VectorBoundingBox3d = vector / interval

  def transformedBy(transformation: Transformation3d): Direction3d = transformation(this)

  def projectedOnto(axis: Axis3d): Vector3d = vector.projectedOnto(axis)

  def projectedOnto(plane: Plane3d): Vector3d = vector.projectedOnto(plane)

  def projectedInto(plane: Plane3d): Vector2d = vector.projectedInto(plane)

  def dot(vector: Vector3d): Double = this.vector.dot(vector)

  def dot(that: Direction3d): Double = this.vector.dot(that.vector)

  def dot(vectorBoundingBox: VectorBoundingBox3d): Interval = vector.dot(vectorBoundingBox)

  def dot(directionBoundingBox: DirectionBoundingBox3d): Interval = vector.dot(directionBoundingBox)

  def cross(vector: Vector3d): Vector3d = this.vector.cross(vector)

  def cross(that: Direction3d): Vector3d = this.vector.cross(that.vector)

  def cross(vectorBoundingBox: VectorBoundingBox3d): VectorBoundingBox3d =
    vector.cross(vectorBoundingBox)

  def cross(directionBoundingBox: DirectionBoundingBox3d): VectorBoundingBox3d =
    vector.cross(directionBoundingBox.vectorBoundingBox)

  def normalDirection: Direction3d = vector.normalDirection

  def angleTo(that: Direction3d): Double = math.acos(this.dot(that))
}

object Direction3d {
  def apply(x: Double, y: Double, z: Double): Direction3d = Direction3d(Vector3d(x, y, z))

  def spherical(azimuth: Double, elevation: Double): Direction3d = {
    val cosElevation = math.cos(elevation)
    val sinElevation = math.sin(elevation)
    val cosAzimuth = math.cos(azimuth)
    val sinAzimuth = math.sin(azimuth)
    Direction3d(cosElevation * cosAzimuth, cosElevation * sinAzimuth, sinElevation)
  }

  def random: Direction3d = random(Random)

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

  val None: Direction3d = Direction3d(0.0, 0.0, 0.0)

  def getNone: Direction3d = Direction3d.None

  val X: Direction3d = Direction3d(1.0, 0.0, 0.0)

  def getX: Direction3d = X

  val Y: Direction3d = Direction3d(0.0, 1.0, 0.0)

  def getY: Direction3d = Y

  val Z: Direction3d = Direction3d(0.0, 0.0, 1.0)

  def getZ: Direction3d = Z
}
