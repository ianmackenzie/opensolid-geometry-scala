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

package org.opensolid.core {

  sealed abstract class CurveExpression1d {
    import CurveExpression1d._

    def derivative: CurveExpression1d

    def unary_- : CurveExpression1d = Negated(this)

    def +(that: CurveExpression1d): CurveExpression1d = (this, that) match {
      case (first, Zero) => first
      case (Zero, second) => second
      case (first, second) if (first == second) => 2 * first
      case (first, Negated(second)) => first - second
      case (Negated(first), second) => second - first
      case (first, second) => Sum(first, second)
    }

    def -(that: CurveExpression1d): CurveExpression1d = (this, that) match {
      case (first, Zero) => first
      case (Zero, second) => -second
      case (first, second) if (first == second) => Zero
      case (first, Negated(second)) => first + second
      case (first, second) => Difference(first, second)
    }

    def *(that: CurveExpression1d): CurveExpression1d = (this, that) match {
      case (_, Zero) => Zero
      case (Zero, _) => Zero
      case (first, One) => first
      case (One, second) => second
      case (first, NegativeOne) => -first
      case (NegativeOne, second) => -second
      case (first, second) if (first == second) => first.squared
      case (first, second) => Product(first, second)
    }

    def /(that: CurveExpression1d): CurveExpression1d = Quotient(this, that)

    def squared: CurveExpression1d = Squared(this)
  }

  object CurveExpression1d {
    def apply(value: Double): CurveExpression1d = Constant(value)

    val Zero: CurveExpression1d = Constant(0.0)

    val One: CurveExpression1d = Constant(1.0)

    val NegativeOne: CurveExpression1d = Constant(-1.0)

    val Parameter: CurveExpression1d = Identity

    case class Constant(value: Double) extends CurveExpression1d {
      private[this] val interval = Interval(value)

      override def derivative: CurveExpression1d = Zero

      override def unary_- : CurveExpression1d = Constant(-value)
    }

    case object Identity extends CurveExpression1d {
      override def derivative: CurveExpression1d = CurveExpression1d.One
    }

    case class Negated(argument: CurveExpression1d) extends CurveExpression1d {
      override def derivative: CurveExpression1d = -argument.derivative

      override def unary_- : CurveExpression1d = argument
    }

    case class Sum(first: CurveExpression1d, second: CurveExpression1d) extends CurveExpression1d {
      override def derivative: CurveExpression1d = first.derivative + second.derivative
    }

    case class Difference(first: CurveExpression1d, second: CurveExpression1d)
      extends CurveExpression1d {

      override def derivative: CurveExpression1d = first.derivative - second.derivative

      override def unary_- : CurveExpression1d = second - first
    }

    case class Product(first: CurveExpression1d, second: CurveExpression1d)
      extends CurveExpression1d {

      override def derivative: CurveExpression1d =
        first.derivative * second + first * second.derivative
    }

    case class Quotient(first: CurveExpression1d, second: CurveExpression1d)
      extends CurveExpression1d {

      override def derivative: CurveExpression1d =
        (first.derivative * second - first * second.derivative) /
        second.squared
    }

    case class Squared(argument: CurveExpression1d) extends CurveExpression1d {
      override def derivative = 2.0 * argument * argument.derivative
    }
  }
}
