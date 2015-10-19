package org.opensolid.core

final case class Handedness(value: Int) {
  def sign: Sign = Sign(value)

  def unary_- : Handedness = Handedness(-value)

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
