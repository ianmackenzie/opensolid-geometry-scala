package org.opensolid.core

final case class Box2d(x: Interval, y: Interval) extends Bounded2d {
  def components: Array[Interval] = Array(x, y)

  override def bounds: Box2d = this
}

object Box2d {
  def fromComponents[T <% Interval](components: Seq[T]): Box2d = components match {
    case Seq(x, y) => Box2d(x, y)
    case _ => throw new IllegalArgumentException("Box2d requires 2 components")
  }

  val Empty: Box2d = Box2d(Interval.Empty, Interval.Empty)

  val Whole: Box2d = Box2d(Interval.Whole, Interval.Whole)

  val Unit: Box2d = Box2d(Interval.Unit, Interval.Unit)
}
