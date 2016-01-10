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

class Builder {
  import Builder._

  private[this] var fieldCount = 0

  private[this] val constructorArguments = mutable.MutableList[Double]()

  private[this] val constructorAssignments = mutable.MutableList[FieldAssignment]()

  private[this] def newField: Field = {
    val result = Field(fieldCount)
    fieldCount = fieldCount + 1
    result
  }

  def variable(value: Double): Value = {
    val result = newField
    constructorAssignments += FieldAssignmentFromParameter(result, Parameter(initArguments.length))
    constructorArguments += value
    result
  }

  private[this] var temporaryCount = 0

  private[this] def newTemporary: Temporary = {
    val result = Temporary(temporaryCount)
    temporaryCount = temporaryCount + 1
    result
  }

  private[this] val cache = mutable.Map[Expression, Value]()

  private[this] val temporaryAssignments = mutable.MutableList[TemporaryAssignment]()

  private[this] def evaluateField(expression: Expression): Field = {
    val result = newField
    initAssignments += FieldAssignmentFromExpression(result, expression)
    cache(expression) = result
    result
  }

  private[this] def evaluateTemporary(expression: Expression): Temporary = {
    val result = newTemporary
    temporaryAssignments += TemporaryAssignment(result, expression)
    cache(expression) = result
    result
  }

  private[this] def evaluate(expression: Expression): Value = expression match {
    case UnaryExpression(Field(_)) => evaluateField(expression)
    case BinaryExpression(Field(_), Field(_)) => evaluateField(expression)
    case _ => evaluateTemporary(expression)
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

  def result: Result = Result(initArguments, initAssignments, temporaryAssignments)
}

object Builder {
  case class Result(
    initArguments: Seq[Double],
    initAssignments: Seq[FieldAssignment],
    temporaryAssignments: Seq[TemporaryAssignment]
  )
}
