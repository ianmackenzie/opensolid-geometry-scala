package org.opensolid.core

import org.scalacheck._
import org.scalacheck.Prop.BooleanOperators

object TolerantComparisonsSpec extends Properties("TolerantComparisons") {
  val small = 1e-8
  val tinyValues = Gen.chooseNum(-DefaultPrecision, DefaultPrecision)
  val smallValues = Gen.chooseNum(-small, small).suchThat(_.abs > DefaultPrecision)
  val smallPositiveValues = smallValues.suchThat(_ > 0.0)
  val smallNegativeValues = smallValues.suchThat(_ < 0.0)

  property("isLessThanZero:tinyValues") =
    Prop.forAll(tinyValues) {!_.isLessThanZero}

  property("isLessThanZero:smallNegativeValues") =
    Prop.forAll(smallNegativeValues) {_.isLessThanZero}

  property("isLessThanZero:smallPositiveValues") =
    Prop.forAll(smallPositiveValues) {!_.isLessThanZero}

  property("isLessThanZero(small):tinyValues") =
    Prop.forAll(tinyValues) {!_.isLessThanZero(small)}

  property("isLessThanZero(small):smallValues") =
    Prop.forAll(smallValues) {!_.isLessThanZero(small)}

  property("isLessThanOrEqualToZero:tinyValues") =
    Prop.forAll(tinyValues) {_.isLessThanOrEqualToZero}

  property("isLessThanOrEqualToZero:smallNegativeValues") =
    Prop.forAll(smallNegativeValues) {_.isLessThanOrEqualToZero}

  property("isLessThanOrEqualToZero:smallPositiveValues") =
    Prop.forAll(smallPositiveValues) {!_.isLessThanOrEqualToZero}

  property("isLessThanOrEqualToZero(small):tinyValues") =
    Prop.forAll(tinyValues) {_.isLessThanOrEqualToZero(small)}

  property("isLessThanOrEqualToZero(small):smallValues") =
    Prop.forAll(smallValues) {_.isLessThanOrEqualToZero(small)}

  property("isZero:tinyValues") =
    Prop.forAll(tinyValues) {_.isZero}

  property("isZero:smallValues") =
    Prop.forAll(smallValues) {!_.isZero}

  property("isZero(small):tinyValues") =
    Prop.forAll(tinyValues) {_.isZero(small)}

  property("isZero(small):smallValues") =
    Prop.forAll(smallValues) {_.isZero(small)}

  property("isNotZero:tinyValues") =
    Prop.forAll(tinyValues) {!_.isNotZero}

  property("isNotZero:smallValues") =
    Prop.forAll(smallValues) {_.isNotZero}

  property("isNotZero(small):tinyValues") =
    Prop.forAll(tinyValues) {!_.isNotZero(small)}

  property("isNotZero(small):smallValues") =
    Prop.forAll(smallValues) {!_.isNotZero(small)}

  property("isGreaterThanOrEqualToZero:tinyValues") =
    Prop.forAll(tinyValues) {_.isGreaterThanOrEqualToZero}

  property("isGreaterThanOrEqualToZero:smallNegativeValues") =
    Prop.forAll(smallNegativeValues) {!_.isGreaterThanOrEqualToZero}

  property("isGreaterThanOrEqualToZero:smallPositiveValues") =
    Prop.forAll(smallPositiveValues) {_.isGreaterThanOrEqualToZero}

  property("isGreaterThanOrEqualToZero(small):tinyValues") =
    Prop.forAll(tinyValues) {_.isGreaterThanOrEqualToZero(small)}

  property("isGreaterThanOrEqualToZero(small):smallValues") =
    Prop.forAll(smallValues) {_.isGreaterThanOrEqualToZero(small)}

  property("isGreaterThanZero:tinyValues") =
    Prop.forAll(tinyValues) {!_.isGreaterThanZero}

  property("isGreaterThanZero:smallNegativeValues") =
    Prop.forAll(smallNegativeValues) {!_.isGreaterThanZero}

  property("isGreaterThanZero:smallPositiveValues") =
    Prop.forAll(smallPositiveValues) {_.isGreaterThanZero}

  property("isGreaterThanZero(small):tinyValues") =
    Prop.forAll(tinyValues) {!_.isGreaterThanZero(small)}

  property("isGreaterThanZero(small):smallValues") =
    Prop.forAll(smallValues) {!_.isGreaterThanZero(small)}
}
