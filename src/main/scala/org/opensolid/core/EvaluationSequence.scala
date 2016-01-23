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

case class EvaluationSequence[T] private (
  operations: List[EvaluationSequence.Operation],
  arraySize: Int,
  map1d: Map[Expression1d[T], Int],
  map2d: Map[Expression2d[T], (Int, Int)]
) {
  import EvaluationSequence._

  def add(expression: Expression1d[T]): (EvaluationSequence[T], Int) =
    map1d.get(expression) match {
      case Some(index) => (this, index)
      case None => {
        import Expression1d._
        expression match {
          case p: Parameter1d[T] => (this, 0)
          case Constant(value) => append(expression)(index => Initialization1d(index, value))
          case Negation(argument) => {
            val (tempSequence, argumentIndex) = add(argument)
            tempSequence.append(expression)(index => Negation1d(argumentIndex, index))
          }
          case _ => ???
        }
      }
    }

  def add(expression: Expression2d[T]): (EvaluationSequence[T], (Int, Int)) = ???

  private def append(
    expression: Expression1d[T]
  )(
    createOperation: (Int) => Operation
  ): (EvaluationSequence[T], Int) =
    (
      EvaluationSequence[T](
        operations :+ createOperation(arraySize),
        arraySize + 1,
        map1d + (expression -> arraySize),
        map2d
      ),
      arraySize
    )

  private def append(
    expression: Expression2d[T]
  )(
    createOperation: (Int, Int) => Operation
  ): (EvaluationSequence[T], (Int, Int)) =
    (
      EvaluationSequence[T](
        operations :+ createOperation(arraySize, arraySize + 1),
        arraySize + 2,
        map1d,
        map2d + (expression -> (arraySize, arraySize + 1))
      ),
      (arraySize, arraySize + 1)
    )
}

object EvaluationSequence {
  def evaluate[T](expression: Expression1d[T]): EvaluationSequence[T] = {
    val initial = EvaluationSequence[T](List.empty, 0, Map.empty, Map.empty)
    val (evaluationSequence, resultIndex) = initial.add(expression)
    evaluationSequence
  }

  sealed abstract class Operation {
    def execute(values: Array[Double]): Unit

    def execute(values: Array[Interval]): Unit
  }

  case class Initialization1d(index: Int, value: Double) extends Operation {
    override def execute(values: Array[Double]): Unit =
      values(index) = value

    override def execute(values: Array[Interval]): Unit =
      values(index) = Interval.singleton(value)
  }

  case class Negation1d(argumentIndex: Int, resultIndex: Int) extends Operation {
    override def execute(values: Array[Double]): Unit =
      values(resultIndex) = -values(argumentIndex)

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = -values(argumentIndex)
  }

  // case class Sum[T](firstExpression: Expression1d[T], secondExpression: Expression1d[T])
  //   extends Expression1d[T]

  // case class Difference[T](firstExpression: Expression1d[T], secondExpression: Expression1d[T])
  //   extends Expression1d[T] {

  //   override def unary_- : Expression1d[T] =
  //     Difference[T](secondExpression, firstExpression)
  // }

  // case class Product[T](firstExpression: Expression1d[T], secondExpression: Expression1d[T])
  //   extends Expression1d[T]

  // case class Quotient[T](firstExpression: Expression1d[T], secondExpression: Expression1d[T])
  //   extends Expression1d[T]

  // case class Square[T](expression: Expression1d[T]) extends Expression1d[T]

  // case class XComponent2d[T](expression: Expression2d[T]) extends Expression1d[T]

  // case class YComponent2d[T](expression: Expression2d[T]) extends Expression1d[T]

  // case class DotProduct2d[T](firstExpression: Expression2d[T], secondExpression: Expression2d[T])
  //   extends Expression1d[T]

  // case class SquaredNorm2d[T](expression: Expression2d[T]) extends Expression1d[T]

  // case class Norm2d[T](expression: Expression2d[T]) extends Expression1d[T] {
  //   override def squared: Expression1d[T] = SquaredNorm2d(expression)
  // }
}
