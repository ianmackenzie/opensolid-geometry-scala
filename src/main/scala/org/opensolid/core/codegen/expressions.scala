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

package org.opensolid.core.codegen

sealed abstract class Expression

case class Constant(value: Double) extends Expression

class Variable(val value: Double) extends Expression

case class Parameter(index: Int) extends Expression

case class Field(index: Int) extends Expression

case class Temporary(index: Int) extends Expression

sealed abstract class UnaryExpression extends Expression {
  def argument: Expression
}

object UnaryExpression {
  def unapply(expression: UnaryExpression): Option[Expression] = Some(expression.argument)
}

sealed abstract class BinaryExpression extends Expression {
  def firstArgument: Expression
  def secondArgument: Expression
}

object BinaryExpression {
  def unapply(expression: BinaryExpression): Option[(Expression, Expression)] =
    Some((expression.firstArgument, expression.secondArgument))
}

case class Negation(argument: Expression) extends UnaryExpression

case class Sum(firstArgument: Expression, secondArgument: Expression)
  extends BinaryExpression

case class Difference(firstArgument: Expression, secondArgument: Expression)
  extends BinaryExpression

case class Product(firstArgument: Expression, secondArgument: Expression)
  extends BinaryExpression

case class Quotient(firstArgument: Expression, secondArgument: Expression)
  extends BinaryExpression

case class Square(argument: Expression) extends UnaryExpression

case class SquareRoot(argument: Expression) extends UnaryExpression

case class Sine(argument: Expression) extends UnaryExpression

case class Cosine(argument: Expression) extends UnaryExpression
