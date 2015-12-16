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

object Language {
  def valueName(value: Value): String = value match {
    case Constant(x) => x.toString
    case Parameter(index) => s"p$index"
    case Temporary(index) => s"t$index"
  }

  def expressionString(expression: Expression): String = expression match {
    case Negation(value) => s"-${valueName(value)}"
    case Sum(first, second) => s"${valueName(first)} + ${valueName(second)}"
    case Difference(first, second) => s"${valueName(first)} - ${valueName(second)}"
    case Product(first, second) => s"${valueName(first)} * ${valueName(second)}"
    case Quotient(first, second) => s"${valueName(first)} / ${valueName(second)}"
    case Square(value) => s"${valueName(value)} * ${valueName(value)}"
    case SquareRoot(value) => s"math.sqrt(${valueName(value)})"
    case Sine(value) => s"math.sin(${valueName(value)})"
    case Cosine(value) => s"math.cos(${valueName(value)})"
  }

  def body(assignments: Seq[Assignment]): Seq[String] = {
    assignments.map(
      assignment => {
        s"  val ${valueName(assignment.result)} = ${expressionString(assignment.expression)}"
      }
    )
  }

  def curveFunction1d(assignments: Seq[Assignment], result: Value): String = {
    val prefix = "(p0: Double) => {\n"
    val suffix = s"\n  ${valueName(result)}\n}"
    val source = body(assignments).mkString(prefix, "\n", suffix)
    source
  }
}
