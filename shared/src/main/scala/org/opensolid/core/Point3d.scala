/*******************************************************************************
*                                                                              *
*  OpenSolid is a generic library for the representation and manipulation of   *
*  geometric objects such as points, curves, surfaces, and volumes.            *
*                                                                              *
*  Copyright 2007-2015 by Ian Mackenzie                                        *
*  ian.e.mackenzie@gmail.com                                                   *
*                                                                              *
*  This Source Code Form is subject to the terms of the Mozilla Public         *
*  License, v. 2.0. If a copy of the MPL was not distributed with this file,   *
*  you can obtain one at http://mozilla.org/MPL/2.0/.                          *
*                                                                              *
*******************************************************************************/

package org.opensolid.core

final case class Point3d(x: Double, y: Double, z: Double)
  extends Bounded3d with Transformable3d[Point3d] with Scalable3d[Point3d] {
  
  def components: Array[Double] = Array(x, y, z)

  def component(index: Int): Double = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Point3d")
  }

  override def bounds: Box3d = Box3d(Interval(x), Interval(y), Interval(z))

  override def transformedBy(transformation: Transformation3d): Point3d = {
    transformation(this)
  }

  override def scaledAbout(point: Point3d, scale: Double): Point3d = point + scale * (this - point)

  def +(vector: Vector3d): Point3d = Point3d(x + vector.x, y + vector.y, z + vector.z)

  def +(vectorBox: VectorBox3d): Box3d = Box3d(x + vectorBox.x, y + vectorBox.y, z + vectorBox.z)

  def -(vector: Vector3d): Point3d = Point3d(x - vector.x, y - vector.y, z - vector.z)

  def -(vectorBox: VectorBox3d): Box3d = Box3d(x - vectorBox.x, y - vectorBox.y, z - vectorBox.z)

  def -(that: Point3d): Vector3d = Vector3d(x - that.x, y - that.y, z - that.z)

  def -(box: Box3d): VectorBox3d = VectorBox3d(x - box.x, y - box.y, z - box.z)
}

object Point3d {
  def fromComponents[T <% Double](components: Seq[T]): Point3d = components match {
    case Seq(x, y, z) => Point3d(x, y, z)
    case _ => throw new IllegalArgumentException("Point3d requires 3 components")
  }

  val Origin: Point3d = Point3d(0.0, 0.0, 0.0)
}
