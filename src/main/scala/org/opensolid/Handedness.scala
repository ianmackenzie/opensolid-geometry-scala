package org.opensolid

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport("Handedness")
case class Handedness(val value: Int) extends AnyVal {
  @JSExport
  def sign = Sign(value)

  @JSExport("negated")
  def unary_- = Handedness(-value)

  @JSExport("multipliedBy")
  def *(that: Handedness) = Handedness(value * that.value)

  @JSExport
  def transformedBy(transformation: Transformation2d) = transformation.transform(this)

  @JSExport
  def transformedBy(transformation: Transformation3d) = transformation.transform(this)
}

@JSExport("Handedness_StaticMembers")
object Handedness {
  @JSExport
  def fromSign(sign: Sign) = Handedness(sign.value)

  @JSExport
  def fromSignOf(value: Double) = Handedness(value.signum)

  @JSExport("LEFT_HANDED")
  val LeftHanded = Handedness(-1)

  @JSExport("NONE")
  val None = Handedness(0)

  @JSExport("RIGHT_HANDED")
  val RightHanded = Handedness(1)
}
