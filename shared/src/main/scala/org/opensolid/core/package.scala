package org.opensolid

package object core {
  val DefaultPrecision: Double = 1e-12

  implicit class Scalar(val value: Double) extends AnyVal {
    def isLessThanZero(precision: Double): Boolean = value < -precision

    def isLessThanZero: Boolean = value < -DefaultPrecision

    def isLessThanOrEqualToZero(precision: Double): Boolean = value <= precision

    def isLessThanOrEqualToZero: Boolean = value <= DefaultPrecision

    def isZero(precision: Double): Boolean = value >= -precision && value <= precision

    def isZero: Boolean = value >= -DefaultPrecision && value <= DefaultPrecision

    def isNotZero(precision: Double): Boolean = value < -precision || value > precision

    def isNotZero: Boolean = value < -DefaultPrecision || value > DefaultPrecision

    def isGreaterThanOrEqualToZero(precision: Double): Boolean = value >= -precision

    def isGreaterThanOrEqualToZero: Boolean = value >= -DefaultPrecision

    def isGreaterThanZero(precision: Double): Boolean = value > precision

    def isGreaterThanZero: Boolean = value > DefaultPrecision
  
    def +(interval: Interval): Interval = {
      Interval(value + interval.lowerBound, value + interval.upperBound)
    }

    def -(interval: Interval): Interval = {
      Interval(value - interval.upperBound, value - interval.lowerBound)
    }
    
    def *(interval: Interval): Interval = interval * value

    def /(interval: Interval): Interval = {
      if (interval.isEmpty) {
        Interval.Empty
      } else if (interval.lowerBound > 0.0) {
        if (value >= 0.0) {
          Interval(value / interval.upperBound, value / interval.lowerBound)
        } else {
          Interval(value / interval.lowerBound, value / interval.upperBound)
        }
      } else if (interval.upperBound < 0.0) {
        if (value >= 0.0) {
          Interval(value / interval.upperBound, value / interval.lowerBound)
        } else {
          Interval(value / interval.lowerBound, value / interval.upperBound)
        }
      } else if (value == 0.0) {
        Interval(0.0)
      } else {
        Interval.Whole
      }
    }

    def *(vector: Vector2d): Vector2d = vector * value

    def *(vector: Vector3d): Vector3d = vector * value

    def *(direction: Direction2d): Vector2d = direction * value

    def *(direction: Direction3d): Vector3d = direction * value
  }
}
