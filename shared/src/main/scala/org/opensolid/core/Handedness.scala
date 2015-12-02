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

final case class Handedness(sign: Sign) {
  def unary_- : Handedness = this match {
    case Handedness.Right => Handedness.Left
    case Handedness.Left => Handedness.Right
    case _ => Handedness.None
  }

  def *(sign: Sign): Handedness = sign match {
    case Sign.Positive => this
    case Sign.Negative => -this
    case _ => Handedness.None
  }

  def *(that: Handedness): Handedness = this * that.sign

  def transformedBy(transformation: Transformation2d): Handedness = transformation(this)

  def transformedBy(transformation: Transformation3d): Handedness = transformation(this)
}

object Handedness {
  val Left: Handedness = Handedness(Sign.Negative)

  val None: Handedness = Handedness(Sign.None)

  val Right: Handedness = Handedness(Sign.Positive)
}
