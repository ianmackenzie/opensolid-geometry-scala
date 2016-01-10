//////////////////////////////////////////////////////////////////////////////
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

object Compiler {
  def compile(results: Vector[Expression]): CompiledFunction = {
    ???
  }

  private[this] val cache = mutable.Map[Vector[Expression], CompiledFunction]

  def variableNumbering(expressionTree: ExpressionTree): Map[Variable, Int] = ???

  def variableNumbering(
    expression: Expression,
    currentNumbering: Map[Variable, Int]
  ): Map[Variable, Int] = expression match {
    case Constant(_) => currentNumbering
    case variable: Variable =>
      if (currentNumbering.contains(variable)) {
        currentNumbering
      } else {
        currentNumbering + (variable -> currentNumbering.size)
      }
    case Parameter(_) => currentNumbering
    case Field(_) => currentNumbering
    case Temporary(_) => currentNumbering
    case unaryExpression: UnaryExpression =>
      variableNumbering(unaryExpression.argument, currentNumbering)
    case binaryExpression: BinaryExpression =>
      variableNumbering(
        binaryExpression.secondArgument,
        variableNumbering(
          binaryExpression.firstArgument,
          currentNumbering
        )
      )
  }

  def assignFields(results: Vector[Expression]): (Vector[Expression], Vector[Double]) = {
    val (expressions, variableValues, fieldAssignments) = results.foldLeft(
      (Vector[Expression].empty, Vector[Double].empty, Map[Variable, Field].empty)
    )(
      (accumulated, expression) => {
        val (expressions, variableValues, fieldAssignments) = accumulated
        val (newExpression, updatedVariableValues, updatedFieldAssignments) =
          assignFields(expression, variableValues, fieldAssignments)
        (expressions :+ newExpression, updatedVariableValues, updatedFieldAssignments)
      }
    )
    (expressions, variableValues)
  }

  private[this] def assignFields(
    expression: Expression,
    variableValues: Vector[Double],
    fieldAssignments: Map[Variable, Field]
  ): (Expression, Vector[Double], Map[Variable, Field]) = expression match {
    case Constant(_) => (expression, variableValues, fieldAssignments)
    case variable: Variable => {
      fieldAssignments.get(variable) match {
        Some(field) => (field, variableValues, fieldAssignments)
        None => {
          val field = Field(fieldAssignments.size)
          (field, variableValues :+ variable.value, fieldAssignments + (variable -> field))
        }
      }
    }
    case Parameter(_) => (expression, variableValues, fieldAssignments)
    case Field(_) => throw internalError
    case Temporary(_) => throw internalError
    case unaryExpression: UnaryExpression => {
      val (newArgument, updatedVariableValues, updatedFieldAssignments) =
        assignFields(unaryExpression.argument, variableValues, fieldAssignments)
      val newExpression = replaceArgument(unaryExpression, newArgument)
      (newExpression, updatedVariableValues, updatedFieldAssignments)
    }
    case binaryExpression: BinaryExpression => {
      val (newFirstArgument, tempVariableValues, tempFieldAssignments) =
        assignFields(binaryExpression.firstArgument, variableValues, fieldAssignments)
      val (newSecondArgument, updatedFieldValues, updatedFieldAssignments) =
        assignFields(binaryExpression.secondArgument, tempFieldValues, tempFieldAssignments)
      val newExpression = replaceArguments(binaryExpression, newFirstArgument, newSecondArgument)
      (newExpression, updatedVariableValues, updatedFieldAssignments)
    }
  }

  private[this] val internalError =
    new IllegalStateException("Internal error in expression compiler")

  def replaceArgument(unaryExpression: UnaryExpression, newArgument: Expression): UnaryExpression =
    unaryExpression match {
      case Negation(_) => Negation(newArgument)
      case Square(_) => Square(newArgument)
      case Sine(_) => Sine(newArgument)
      case Cosine(_) => Cosine(newArgument)
    }

  def replaceArguments(
    binaryExpression: BinaryExpression,
    newFirstArgument: Expression,
    newSecondArgument: Expression
  ): BinaryExpression =
    binaryExpression match {
      case Sum(_, _) => Sum(newFirstArgument, newSecondArgument)
      case Difference(_, _) => Difference(newFirstArgument, newSecondArgument)
      case Product(_, _) => Product(newFirstArgument, newSecondArgument)
      case Quotient(_, _) => Quotient(newFirstArgument, newSecondArgument)
    }
}
