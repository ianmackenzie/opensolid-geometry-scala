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

  def isEmpty: Boolean = x.isEmpty || y.isEmpty || z.isEmpty

  def isWhole: Boolean = x.isWhole && y.isWhole && z.isWhole

  def isSingleton: Boolean = x.isSingleton && y.isSingleton && z.isSingleton

  def hull(point: Point3d): Box3d = Box3d(x.hull(point.x), y.hull(point.y), z.hull(point.z))

  def hull(that: Box3d): Box3d =
    Box3d(this.x.hull(that.x), this.y.hull(that.y), this.z.hull(that.z))

  def intersection(that: Box3d): Box3d = {
    val x = this.x.intersection(that.x)
    val y = this.y.intersection(that.y)
    val z = this.z.intersection(that.z)
    if (x.isEmpty || y.isEmpty || z.isEmpty) Box3d.Empty else Box3d(x, y, z)
  }

  def overlaps(that: Box3d): Boolean =
    this.x.overlaps(that.x) && this.y.overlaps(that.y) && this.z.overlaps(that.z)

  def overlaps(that: Box3d, tolerance: Double): Boolean =
    this.x.overlaps(that.x, tolerance) &&
    this.y.overlaps(that.y, tolerance) &&
    this.z.overlaps(that.z, tolerance)

  def contains(point: Point3d): Boolean =
    x.contains(point.x) && y.contains(point.y) && z.contains(point.z)

  def contains(point: Point3d, tolerance: Double): Boolean =
    x.contains(point.x, tolerance) &&
    y.contains(point.y, tolerance) &&
    z.contains(point.z, tolerance)

  def contains(that: Box3d): Boolean =
    this.x.contains(that.x) && this.y.contains(that.y) && this.z.contains(that.z)

  def contains(that: Box3d, tolerance: Double): Boolean =
    this.x.contains(that.x, tolerance) &&
    this.y.contains(that.y, tolerance) &&
    this.z.contains(that.z, tolerance)

  def +(vector: Vector3d): Box3d = Box3d(x + vector.x, y + vector.y, z + vector.z)

  def +(vectorBox: VectorBox3d): Box3d = Box3d(x + vectorBox.x, y + vectorBox.y, z + vectorBox.z)

  def -(vector: Vector3d): Box3d = Box3d(x - vector.x, y - vector.y, z - vector.z)

  def -(vectorBox: VectorBox3d): Box3d = Box3d(x - vectorBox.x, y - vectorBox.y, z - vectorBox.z)

  def -(point: Point3d): VectorBox3d = VectorBox3d(x - point.x, y - point.y, z - point.z)

  def -(that: Box3d): VectorBox3d = VectorBox3d(this.x - that.x, this.y - that.y, this.z - that.z)
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
