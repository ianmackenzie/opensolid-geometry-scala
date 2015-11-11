package org.opensolid.core

final case class Box3d(x: Interval, y: Interval, z: Interval) extends Bounded3d {
  def components: Array[Interval] = Array(x, y, z)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Box3d")
  }

  override def bounds: Box3d = this
}

object Box3d {
  def fromComponents[T <% Interval](components: Seq[T]): Box3d = components match {
    case Seq(x, y, z) => Box3d(x, y, z)
    case _ => throw new IllegalArgumentException("Box3d requires 3 components")
  }

  val Empty: Box3d = Box3d(Interval.Empty, Interval.Empty, Interval.Empty)

  val Whole: Box3d = Box3d(Interval.Whole, Interval.Whole, Interval.Whole)

  val Unit: Box3d = Box3d(Interval.Unit, Interval.Unit, Interval.Unit)
}
