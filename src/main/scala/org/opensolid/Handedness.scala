package org.opensolid

import scala.scalajs.js
import js.annotation.JSExport
import js.annotation.JSExportAll

class Handedness private (val value: Int) extends AnyVal {
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
  val LeftHanded = new Handedness(-1)

  @JSExport("NONE")
  val None = new Handedness(0)

  @JSExport("RIGHT_HANDED")
  val RightHanded = new Handedness(1)

  private val constants = Array(LeftHanded, None, RightHanded)
  private[opensolid] def apply(value: Int) = constants(value + 1)
}
