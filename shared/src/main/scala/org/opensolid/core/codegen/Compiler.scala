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

import scala.collection.mutable

import org.opensolid.core._

class Compiler {
  private[this] var temporaryIndex = 0

  private[this] def newTemporary: Temporary = {
    val result = Temporary(temporaryIndex)
    temporaryIndex = temporaryIndex + 1
    result
  }

  private[this] val cache = mutable.Map[Expression, Value]()

  private[Compiler] val assignments = mutable.MutableList[Assignment]()

  private[this] def evaluate(expression: Expression): Temporary = {
    val result = newTemporary
    assignments += Assignment(result, expression)
    cache(expression) = result
    result
  }

  private[this] def cachedValueOf(expression: Expression): Option[Value] = expression match {
    case Sum(first, second) =>
      cache.get(Sum(first, second)).orElse(cache.get(Sum(second, first)))
    case Product(first, second) =>
      cache.get(Product(first, second)).orElse(cache.get(Product(second, first)))
    case _ => cache.get(expression)
  }

  def valueOf(expression: Expression): Value =
    cachedValueOf(expression).getOrElse(evaluate(expression))

  def negation(value: Value): Value = valueOf(Negation(value))

  def sum(first: Value, second: Value): Value = valueOf(Sum(first, second))

  def difference(first: Value, second: Value): Value = valueOf(Difference(first, second))

  def product(first: Value, second: Value): Value = valueOf(Product(first, second))

  def quotient(first: Value, second: Value): Value = valueOf(Quotient(first, second))

  def square(value: Value): Value = valueOf(Square(value))

  def sqrt(value: Value): Value = valueOf(SquareRoot(value))

  def sin(value: Value): Value = valueOf(Sine(value))

  def cos(value: Value): Value = valueOf(Cosine(value))
}

object Compiler {
  def curveFunction1d(expression: CurveExpression1d): String = {
    val compiler = new Compiler
    val result = expression.evaluate(compiler)
    Language.curveFunction1d(compiler.assignments, result)
  }
}
