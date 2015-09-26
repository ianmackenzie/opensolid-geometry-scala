package org.opensolid

import scala.scalajs.js
import js.annotation.JSExport
import js.annotation.JSExportAll

@JSExport("Sign")
case class Sign(val value: Int) extends AnyVal {
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
  val Negative = Sign(-1)

  @JSExport("ZERO")
  val Zero = Sign(0)

  @JSExport("POSITIVE")
  val Positive = Sign(1)
}
