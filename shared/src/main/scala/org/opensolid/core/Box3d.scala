package org.opensolid.core

final case class Box3d(x: Interval, y: Interval, z: Interval) extends Bounded3d {
  override def bounds: Box3d = this
}

object Box3d {
  val Empty: Box3d = Box3d(Interval.Empty, Interval.Empty, Interval.Empty)

  val Whole: Box3d = Box3d(Interval.Whole, Interval.Whole, Interval.Whole)

  val Unit: Box3d = Box3d(Interval.Unit, Interval.Unit, Interval.Unit)
}
