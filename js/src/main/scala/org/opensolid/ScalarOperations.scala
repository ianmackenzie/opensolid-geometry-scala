package org.opensolid

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport("ScalarOperations")
@JSExportAll
object ScalarOperations {
  def sum(value: Double, interval: Interval): Interval = value + interval

  def difference(value: Double, interval: Interval): Interval = value - interval

  def product(value: Double, interval: Interval): Interval = value * interval

  def quotient(value: Double, interval: Interval): Interval = value / interval

  def product(value: Double, vector: Vector2d): Vector2d = value * vector

  def product(value: Double, vector: Vector3d): Vector3d = value * vector
}
