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

import scala.beans.BeanProperty

final case class Sign private (value: Int) {
  def unary_- : Sign = this match {
    case Sign.Positive => Sign.Negative
    case Sign.Negative => Sign.Positive
    case _ => Sign.None
  }

  def negated: Sign = -this

  def *(that: Sign): Sign = that match {
    case Sign.Positive => this
    case Sign.Negative => -this
    case _ => Sign.None
  }

  def times(that: Sign): Sign = this * that

  def *(value: Double): Double = this.value * value

  def times(value: Double): Double = this * value

  def *(interval: Interval): Interval = interval * this

  def times(interval: Interval): Interval = this * interval

  def *(handedness: Handedness): Handedness = handedness * this

  def times(handedness: Handedness): Handedness = this * handedness

  def *(vector: Vector2d): Vector2d = vector * this

  def times(vector: Vector2d): Vector2d = this * vector

  def *(vector: Vector3d): Vector3d = vector * this

  def times(vector: Vector3d): Vector3d = this * vector

  def *(direction: Direction2d): Direction2d = direction * this

  def times(direction: Direction2d): Direction2d = this * direction

  def *(direction: Direction3d): Direction3d = direction * this

  def times(direction: Direction3d): Direction3d = this * direction

  def *(vectorBoundingBox: VectorBoundingBox2d): VectorBoundingBox2d = vectorBoundingBox * this

  def times(vectorBoundingBox: VectorBoundingBox2d): VectorBoundingBox2d = this * vectorBoundingBox

  def *(vectorBoundingBox: VectorBoundingBox3d): VectorBoundingBox3d = vectorBoundingBox * this

  def times(vectorBoundingBox: VectorBoundingBox3d): VectorBoundingBox3d = this * vectorBoundingBox

  def *(directionBoundingBox: DirectionBoundingBox2d): DirectionBoundingBox2d =
    directionBoundingBox * this

  def times(directionBoundingBox: DirectionBoundingBox2d): DirectionBoundingBox2d =
    this * directionBoundingBox

  def *(directionBoundingBox: DirectionBoundingBox3d): DirectionBoundingBox3d =
    directionBoundingBox * this

  def times(directionBoundingBox: DirectionBoundingBox3d): DirectionBoundingBox3d =
    this * directionBoundingBox
}

object Sign {
  def of(value: Double): Sign = value.signum match {
    case 1 => Sign.Positive
    case -1 => Sign.Negative
    case _ => Sign.None
  }

  @BeanProperty
  val Negative: Sign = Sign(-1)

  @BeanProperty
  val None: Sign = Sign(0)

  @BeanProperty
  val Positive: Sign = Sign(1)
}