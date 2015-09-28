package org

package object opensolid {
  implicit class TolerantComparisons(val value: Double) extends AnyVal {
    def isLessThanZero(precision: Double = 1e-12) = value < -precision
    def isLessThanOrEqualToZero(precision: Double = 1e-12) = value <= precision
    def isZero(precision: Double = 1e-12) = -precision <= value && value <= precision
    def isGreaterThanOrEqualToZero(precision: Double = 1e-12) = value >= -precision
    def isGreaterThanZero(precision: Double = 1e-12) = value > precision
  }
}
