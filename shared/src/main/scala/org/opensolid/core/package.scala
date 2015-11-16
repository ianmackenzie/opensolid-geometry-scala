package org.opensolid

package object core {
  implicit class Scalar(val value: Double) extends AnyVal {
    def isZero(tolerance: Double): Boolean = value >= -tolerance && value <= tolerance

    def isNotZero(tolerance: Double): Boolean = value < -tolerance || value > tolerance
  
    def *(sign: Sign): Double = value * sign.value

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
