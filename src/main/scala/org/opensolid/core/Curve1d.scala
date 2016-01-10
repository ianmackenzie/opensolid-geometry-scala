////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  OpenSolid is a generic library for the representation and manipulation    //
//  of geometric objects such as points, curves, surfaces, and volumes.       //
//                                                                            //
//  Copyright 2007-2015 by Ian Mackenzie                                      //
//  ian.e.mackenzie@gmail.com                                                 //
//                                                                            //
//  This Source Code Form is subject to the terms of the Mozilla Public       //
//  License, v. 2.0. If a copy of the MPL was not distributed with this file, //
//  you can obtain one at http://mozilla.org/MPL/2.0/.                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

package org.opensolid.core

abstract class Curve1d {
  import Curve1d._

  def evaluate(t: Double): Double

  def derivative: Curve1d

  def unary_- : Curve1d = Negation(this)

  final def negated: Curve1d = -this

  final def +(that: Curve1d): Curve1d = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue + secondValue)
    case (curve, Zero) => curve
    case (Zero, curve) => curve
    case (first, second) if (first == second) => Constant(2) * first
    case (first, Negation(second)) => first - second
    case (Negation(first), second) => second - first
    case _ => Sum(this, that)
  }

  final def plus(that: Curve1d): Curve1d = this + that

  final def -(that: Curve1d): Curve1d = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue - secondValue)
    case (curve, Zero) => curve
    case (Zero, curve) => -curve
    case (first, second) if (first == second) => Zero
    case (first, Negation(second)) => first + second
    case _ => Difference(this, that)
  }

  final def minus(that: Curve1d): Curve1d = this - that

  final def *(that: Curve1d): Curve1d = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue * secondValue)
    case (_, Zero) => Zero
    case (Zero, _) => Zero
    case (curve, One) => curve
    case (One, curve) => curve
    case (curve, NegativeOne) => -curve
    case (NegativeOne, curve) => -curve
    case (first, second) if (first == second) => first.squared
    case (Quotient(a, b), Quotient(c, d)) => (a * c) / (b * d)
    case _ => Product(this, that)
  }

  final def times(that: Curve1d): Curve1d = this * that

  final def /(that: Curve1d): Curve1d = (this, that) match {
    case (_, Zero) => throw new ArithmeticException("Division by zero")
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue / secondValue)
    case (Zero, _) => Zero
    case (curve, One) => curve
    case (curve, NegativeOne) => -curve
    case (curve, Constant(value)) => Constant(1.0 / value) * curve
    case (curve, Quotient(numerator, denominator)) => curve * denominator / numerator
    case _ => Quotient(this, that)
  }

  final def dividedBy(that: Curve1d): Curve1d = this / that

  def squared: Curve1d = Square(this)
}

object Curve1d {
  val Zero: Curve1d = Constant(0.0)

  val One: Curve1d = Constant(1.0)

  val NegativeOne: Curve1d = Constant(-1.0)

  val Parameter: Curve1d = Identity

  case class Constant(value: Double) extends Curve1d {
    override def evaluate(t: Double): Double = value

    override def derivative: Curve1d = Zero

    override def unary_- : Curve1d = Constant(-value)

    override def squared: Curve1d = Constant(value * value)
  }

  case object Identity extends Curve1d {
    override def evaluate(t: Double): Double = t

    override def derivative: Curve1d = Curve1d.One
  }

  case class Negation(curve: Curve1d) extends Curve1d {
    override def evaluate(t: Double): Double = -curve.evaluate(t)

    override def derivative: Curve1d = -curve.derivative

    override def unary_- : Curve1d = curve

    override def squared: Curve1d = curve.squared
  }

  case class Sum(firstCurve: Curve1d, secondCurve: Curve1d) extends Curve1d {
    override def evaluate(t: Double): Double = firstCurve.evaluate(t) + secondCurve.evaluate(t)

    override def derivative: Curve1d = firstCurve.derivative + secondCurve.derivative
  }

  case class Difference(firstCurve: Curve1d, secondCurve: Curve1d) extends Curve1d {
    override def evaluate(t: Double): Double = firstCurve.evaluate(t) - secondCurve.evaluate(t)

    override def derivative: Curve1d = firstCurve.derivative - secondCurve.derivative

    override def unary_- : Curve1d = Difference(secondCurve, firstCurve)
  }

  case class Product(firstCurve: Curve1d, secondCurve: Curve1d) extends Curve1d {
    override def evaluate(t: Double): Double = firstCurve.evaluate(t) * secondCurve.evaluate(t)

    override def derivative: Curve1d =
      firstCurve.derivative * secondCurve + firstCurve * secondCurve.derivative
  }

  case class Quotient(firstCurve: Curve1d, secondCurve: Curve1d) extends Curve1d {
    override def evaluate(t: Double): Double = firstCurve.evaluate(t) / secondCurve.evaluate(t)

    override def derivative: Curve1d =
      (firstCurve.derivative * secondCurve - firstCurve * secondCurve.derivative) /
      secondCurve.squared
  }

  case class Square(curve: Curve1d) extends Curve1d {
    override def evaluate(t: Double): Double = {
      val temp = curve.evaluate(t)
      temp * temp
    }

    override def derivative: Curve1d = Constant(2.0) * curve * curve.derivative
  }
}
