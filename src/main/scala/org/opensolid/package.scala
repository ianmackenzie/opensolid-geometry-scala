package org

package object opensolid {
  val DefaultPrecision = 1e-12

  implicit class TolerantComparisons(val value: Double) extends AnyVal {
    def isLessThanZero(precision: Double) = value < -precision
    def isLessThanZero = value < -DefaultPrecision

    def isLessThanOrEqualToZero(precision: Double) = value <= precision
    def isLessThanOrEqualToZero = value <= DefaultPrecision

    def isZero(precision: Double) = -precision <= value && value <= precision
    def isZero = -DefaultPrecision <= value && value <= DefaultPrecision

    def isGreaterThanOrEqualToZero(precision: Double) = value >= -precision
    def isGreaterThanOrEqualToZero = value >= -DefaultPrecision

    def isGreaterThanZero(precision: Double) = value > precision
    def isGreaterThanZero = value > DefaultPrecision
  }
}
