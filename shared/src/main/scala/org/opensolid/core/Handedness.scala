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

final case class Handedness(value: Int) {
  def sign: Sign = Sign(value)

  def unary_- : Handedness = Handedness(-value)

  def *(sign: Sign): Handedness = Handedness(value * sign.value)

  def *(that: Handedness): Handedness = Handedness(value * that.value)

  def transformedBy(transformation: Transformation2d): Handedness = transformation(this)

  def transformedBy(transformation: Transformation3d): Handedness = transformation(this)
}

object Handedness {
  def fromSign(sign: Sign): Handedness = Handedness(sign.value)

  def fromSignOf(value: Double): Handedness = Handedness(value.signum)

  val Left: Handedness = Handedness(-1)

  val None: Handedness = Handedness(0)

  val Right: Handedness = Handedness(1)
}
