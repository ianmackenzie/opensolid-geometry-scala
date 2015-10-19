package org.opensolid.core

trait Bounded1d {
  def bounds: Interval
}

trait Bounded2d {
  def bounds: Box2d
}

trait Bounded3d {
  def bounds: Box3d
}
