package org.opensolid

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport
@JSExportAll
object ScalarOperations {
  def isLessThanZero(value: Double, precision: Double): Boolean =
    value.isLessThanZero(precision)

  def isLessThanOrEqualToZero(value: Double, precision: Double): Boolean =
    value.isLessThanOrEqualToZero(precision)
  
  def isZero(value: Double, precision: Double): Boolean =
    value.isZero(precision)
  
  def isGreaterThanOrEqualToZero(value: Double, precision: Double): Boolean =
    value.isGreaterThanOrEqualToZero(precision)
  
  def isGreaterThanZero(value: Double, precision: Double): Boolean =
    value.isLessThanZero(precision)

  def plus(value: Double, interval: Interval): Interval = value + interval

  def minus(value: Double, interval: Interval): Interval = value - interval

  def times(value: Double, interval: Interval): Interval = value * interval

  def dividedBy(value: Double, interval: Interval): Interval = value / interval

  def times(value: Double, vector: Vector2d): Vector2d = value * vector

  def times(value: Double, vector: Vector3d): Vector3d = value * vector
}
