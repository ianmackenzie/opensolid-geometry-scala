package org.opensolid.core

final case class Box2d(x: Interval, y: Interval) extends Bounded2d {
  override def bounds: Box2d = this
}

object Box2d {
  val Empty: Box2d = Box2d(Interval.Empty, Interval.Empty)

  val Whole: Box2d = Box2d(Interval.Whole, Interval.Whole)

  val Unit: Box2d = Box2d(Interval.Unit, Interval.Unit)
}
