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

sealed abstract class UnaryExpression extends Expression {
  def argument: Value
}

object UnaryExpression {
  def unapply(expression: UnaryExpression): Option[Value] =
    Some(expression.argument)
}

sealed abstract class BinaryExpression extends Expression {
  def firstArgument: Value
  def secondArgument: Value
}

object BinaryExpression {
  def unapply(expression: BinaryExpression): Option[(Value, Value)] =
    Some((expression.firstArgument, expression.secondArgument))
}

case class Negation(argument: Value) extends UnaryExpression

case class Sum(firstArgument: Value, secondArgument: Value) extends BinaryExpression

case class Difference(firstArgument: Value, secondArgument: Value) extends BinaryExpression

case class Product(firstArgument: Value, secondArgument: Value) extends BinaryExpression

case class Quotient(firstArgument: Value, secondArgument: Value) extends BinaryExpression

case class Square(argument: Value) extends UnaryExpression

case class SquareRoot(argument: Value) extends UnaryExpression

case class Sine(argument: Value) extends UnaryExpression

case class Cosine(argument: Value) extends UnaryExpression
