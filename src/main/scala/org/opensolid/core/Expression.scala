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

object Expression {
  def compile[T](expression: Expression1d[T]): (T) => Double = ???

  def compile[T, R](expression: Expression2d[T]): (T) => R = ???

  private[this] def append[T](
    operations: Operations[T],
    expression: Expression1d[T]
  ): Operations[T] = ???

  private[this] def append[T](
    operations: Operations[T],
    expression: Expression2d[T]
  ): Operations[T] = ???

  private[this] case class Operations[T](
    operations: List[Operation],
    arraySize: Int,
    map1d: Map[Expression1d[T], Int],
    map2d: Map[Expression2d[T], Int]
  )

  private[this] object Operations {
    def empty[T]: Operations[T] = Operations[T](List.empty, 0, Map.empty, Map.empty)
  }

  private[this] sealed abstract class Operation {
    def execute(values: Array[Double]): Unit

    def execute(values: Array[Interval]): Unit
  }

  private[this] case class Negation1d(argumentIndex: Int, resultIndex: Int) extends Operation {
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
