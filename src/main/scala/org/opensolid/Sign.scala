package org.opensolid

import scala.scalajs.js
import js.annotation.JSExport
import js.annotation.JSExportAll

class Sign private (val value: Int) extends AnyVal {
  @JSExport("multipliedBy")
  def *(that: Sign) = Sign(value * that.value)

  @JSExport("negated")
  def unary_- = Sign(-value)
}

@JSExport("Sign_StaticMembers")
object Sign {
  @JSExport
  def of(value: Double) = Sign(value.signum)

  @JSExport("NEGATIVE")
  val Negative = new Sign(-1)

  @JSExport("ZERO")
  val Zero = new Sign(0)

  @JSExport("POSITIVE")
  val Positive = new Sign(1)

  private val constants = Array(Negative, Zero, Positive)
  private[opensolid] def apply(value: Int) = constants(value + 1)
}
