package org.opensolid.core

final case class Box2d(x: Interval, y: Interval) extends Bounded2d {
  def components: Array[Interval] = Array(x, y)

  def component(index: Int): Interval = index match {
    case 0 => x
    case 1 => y
    case _ => throw new IndexOutOfBoundsException(s"Index $index is out of bounds for Box2d")
  }

  override def bounds: Box2d = this

  def isEmpty: Boolean = x.isEmpty || y.isEmpty

  def isWhole: Boolean = x.isWhole && y.isWhole

  def isSingleton: Boolean = x.isSingleton && y.isSingleton

  def hull(point: Point2d): Box2d = Box2d(x.hull(point.x), y.hull(point.y))

  def hull(that: Box2d): Box2d = Box2d(this.x.hull(that.x), this.y.hull(that.y))

  def intersection(that: Box2d): Box2d = {
    val x = this.x.intersection(that.x)
    val y = this.y.intersection(that.y)
    if (x.isEmpty || y.isEmpty) Box2d.Empty else Box2d(x, y)
  }

  def overlaps(that: Box2d): Boolean = this.x.overlaps(that.x) && this.y.overlaps(that.y)

  def overlaps(that: Box2d, tolerance: Double): Boolean =
    this.x.overlaps(that.x, tolerance) && this.y.overlaps(that.y, tolerance)

  def contains(point: Point2d): Boolean = x.contains(point.x) && y.contains(point.y)

  def contains(point: Point2d, tolerance: Double): Boolean =
    x.contains(point.x, tolerance) && y.contains(point.y, tolerance)

  def contains(that: Box2d): Boolean = this.x.contains(that.x) && this.y.contains(that.y)

  def contains(that: Box2d, tolerance: Double): Boolean =
    this.x.contains(that.x, tolerance) && this.y.contains(that.y, tolerance)

  def +(vector: Vector2d): Box2d = Box2d(x + vector.x, y + vector.y)

  def +(vectorBox: VectorBox2d): Box2d = Box2d(x + vectorBox.x, y + vectorBox.y)

  def -(vector: Vector2d): Box2d = Box2d(x - vector.x, y - vector.y)

  def -(vectorBox: VectorBox2d): Box2d = Box2d(x - vectorBox.x, y - vectorBox.y)

  def -(point: Point2d): VectorBox2d = VectorBox2d(x - point.x, y - point.y)

  def -(that: Box2d): VectorBox2d = VectorBox2d(this.x - that.x, this.y - that.y)

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
