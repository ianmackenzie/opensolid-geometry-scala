package org

package object opensolid {
  val DefaultPrecision: Double = 1e-12

  implicit class TolerantComparisons(val value: Double) extends AnyVal {
    def isLessThanZero(precision: Double): Boolean = value < -precision
    def isLessThanZero: Boolean = value < -DefaultPrecision

    def isLessThanOrEqualToZero(precision: Double): Boolean = value <= precision
    def isLessThanOrEqualToZero: Boolean = value <= DefaultPrecision

    def isZero(precision: Double): Boolean = -precision <= value && value <= precision
    def isZero: Boolean = -DefaultPrecision <= value && value <= DefaultPrecision

    def isGreaterThanOrEqualToZero(precision: Double): Boolean = value >= -precision
    def isGreaterThanOrEqualToZero: Boolean = value >= -DefaultPrecision

    def isGreaterThanZero(precision: Double): Boolean = value > precision
    def isGreaterThanZero: Boolean = value > DefaultPrecision
  }

  implicit class Multiplications(val value: Double) extends AnyVal {
    def *(interval: Interval): Interval = interval * value
    def *(vector: Vector2d): Vector2d = vector * value
    def *(vector: Vector3d): Vector3d = vector * value
  }
}
