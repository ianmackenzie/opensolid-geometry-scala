package org.opensolid

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport("Handedness")
@JSExportAll
final case class Handedness(value: Int) {
  def sign: Sign = Sign(value)

  @JSExport("negated")
  def unary_- : Handedness = Handedness(-value)

  @JSExport("times")
  def *(that: Handedness): Handedness = Handedness(value * that.value)

  def transformedBy(transformation: Transformation): Handedness = transformation.transform(this)
}

@JSExport("Handedness$StaticMembers")
object Handedness {
  @JSExport
  def fromSign(sign: Sign): Handedness = Handedness(sign.value)

  @JSExport
  def fromSignOf(value: Double): Handedness = Handedness(value.signum)

  @JSExport("LEFT")
  val Left: Handedness = Handedness(-1)

  @JSExport("NONE")
  val None: Handedness = Handedness(0)

  @JSExport("RIGHT")
  val Right: Handedness = Handedness(1)
}
