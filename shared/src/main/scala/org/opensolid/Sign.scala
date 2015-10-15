package org.opensolid

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport("Sign")
@JSExportAll
final case class Sign(value: Int) {
  @JSExport("negated")
  def unary_- : Sign = Sign(-value)
  
  @JSExport("times")
  def *(that: Sign): Sign = Sign(value * that.value)

  @JSExport("times")
  def *(handedness: Handedness): Handedness = Handedness(value * handedness.value)

  @JSExport("times")
  def *(unitVector: UnitVector2d): UnitVector2d = {
    UnitVector2d(value * unitVector.x, value * unitVector.y)
  }
  
  @JSExport("times")
  def *(unitVector: UnitVector3d): UnitVector3d = {
    UnitVector3d(value * unitVector.x, value * unitVector.y, value * unitVector.z)
  }
}

@JSExport("Sign$StaticMembers")
object Sign {
  @JSExport
  def of(value: Double): Sign = Sign(value.signum)

  @JSExport("NEGATIVE")
  val Negative: Sign = Sign(-1)

  @JSExport("NONE")
  val None: Sign = Sign(0)

  @JSExport("POSITIVE")
  val Positive: Sign = Sign(1)
}
