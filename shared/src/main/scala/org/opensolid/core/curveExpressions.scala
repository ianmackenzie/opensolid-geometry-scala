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
    def derivative: CurveExpression1d

    def unary_- : CurveExpression1d = expressions.NegatedCurve1d(this)

    def +(that: CurveExpression1d): CurveExpression1d =
      if (this == that) 2 * this else expressions.CurveSum1d(this, that)

    def -(that: CurveExpression1d): CurveExpression1d = expressions.CurveDifference1d(this, that)

    def *(that: CurveExpression1d): CurveExpression1d =
      if (this == that) this.squared else expressions.CurveProduct1d(this, that)

    def /(that: CurveExpression1d): CurveExpression1d = expressions.CurveQuotient1d(this, that)

    def squared: CurveExpression1d = expressions.SquaredCurve1d(this)
  }

  object CurveExpression1d {
    def apply(value: Double): CurveExpression1d = expressions.ConstantCurve1d(value)

    val Zero = CurveExpression1d(0.0)

    val One = CurveExpression1d(1.0)
  }

  case object Parameter1d extends CurveExpression1d {
    override def derivative: CurveExpression1d = CurveExpression1d.One
  }

  package expressions {

    case class ConstantCurve1d(value: Double) extends CurveExpression1d {
      private[this] val interval = Interval(value)

      override def derivative: CurveExpression1d = CurveExpression1d.Zero

      override def unary_- : CurveExpression1d = ConstantCurve1d(-value)
    }

    case class NegatedCurve1d(argument: CurveExpression1d) extends CurveExpression1d {
      override def derivative: CurveExpression1d = -argument.derivative

      override def unary_- : CurveExpression1d = argument
    }

    case class CurveSum1d(firstArgument: CurveExpression1d, secondArgument: CurveExpression1d)
      extends CurveExpression1d {

      override def derivative: CurveExpression1d =
        firstArgument.derivative + secondArgument.derivative
    }

    case class CurveDifference1d(
      firstArgument: CurveExpression1d,
      secondArgument: CurveExpression1d
    ) extends CurveExpression1d {

      override def derivative: CurveExpression1d =
        firstArgument.derivative - secondArgument.derivative

      override def unary_- : CurveExpression1d = secondArgument - firstArgument
    }

    case class CurveProduct1d(
      firstArgument: CurveExpression1d,
      secondArgument: CurveExpression1d
    ) extends CurveExpression1d {

      override def derivative: CurveExpression1d =
        firstArgument.derivative * secondArgument + firstArgument * secondArgument.derivative
    }

    case class CurveQuotient1d(
      firstArgument: CurveExpression1d,
      secondArgument: CurveExpression1d
    ) extends CurveExpression1d {

      override def derivative: CurveExpression1d =
        (firstArgument.derivative * secondArgument - firstArgument * secondArgument.derivative) /
        secondArgument.squared
    }

    case class SquaredCurve1d(argument: CurveExpression1d) extends CurveExpression1d {
      override def derivative = 2.0 * argument * argument.derivative
    }
  }

}
