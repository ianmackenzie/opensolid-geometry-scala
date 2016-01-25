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

  def evaluate(expression: Expression1d[T]): (EvaluationSequence[T], Int) =
    map1d.get(expression) match {
      case Some(index) => (this, index)
      case None => expression match {
        case parameter: Expression1d.Parameter[T] =>
          (this, parameter.index)
        case Expression1d.Constant(value) =>
          append1d(expression, Constant1d(_, value))
        case Expression1d.XComponent2d(expression) => {
          val (withExpression, (xIndex, yIndex)) = this.evaluate(expression)
          (withExpression, xIndex)
        }
        case Expression1d.YComponent2d(expression) => {
          val (withExpression, (xIndex, yIndex)) = this.evaluate(expression)
          (withExpression, yIndex)
        }
        case Expression1d.Negation(argument) => {
          val (withArgument, argumentIndex) = evaluate(argument)
          withArgument.append1d(expression, Negation1d(argumentIndex, _))
        }
        case Expression1d.Sum(firstArgument, secondArgument) => {
          val (withFirst, firstArgumentIndex) = this.evaluate(firstArgument)
          val (withSecond, secondArgumentIndex) = withFirst.evaluate(secondArgument)
          withSecond.append1d(expression, Sum1d(firstArgumentIndex, secondArgumentIndex, _))
        }
        case Expression1d.Difference(firstArgument, secondArgument) => {
          val (withFirst, firstArgumentIndex) = this.evaluate(firstArgument)
          val (withSecond, secondArgumentIndex) = withFirst.evaluate(secondArgument)
          withSecond.append1d(expression, Difference1d(firstArgumentIndex, secondArgumentIndex, _))
        }
        case Expression1d.Product(firstArgument, secondArgument) => {
          val (withFirst, firstArgumentIndex) = this.evaluate(firstArgument)
          val (withSecond, secondArgumentIndex) = withFirst.evaluate(secondArgument)
          withSecond.append1d(expression, Product1d(firstArgumentIndex, secondArgumentIndex, _))
        }
        case Expression1d.Quotient(firstArgument, secondArgument) => {
          val (withFirst, firstArgumentIndex) = this.evaluate(firstArgument)
          val (withSecond, secondArgumentIndex) = withFirst.evaluate(secondArgument)
          withSecond.append1d(expression, Quotient1d(firstArgumentIndex, secondArgumentIndex, _))
        }
        case Expression1d.Square(argument) => {
          val (withArgument, argumentIndex) = evaluate(argument)
          withArgument.append1d(expression, Square(argumentIndex, _))
        }
        case Expression1d.SquareRoot(argument) => {
          val (withArgument, argumentIndex) = evaluate(argument)
          withArgument.append1d(expression, SquareRoot(argumentIndex, _))
        }
        case Expression1d.DotProduct2d(firstArgument, secondArgument) => {
          val (withFirst, firstArgumentIndices) = this.evaluate(firstArgument)
          val (withSecond, secondArgumentIndices) = withFirst.evaluate(secondArgument)
          withSecond.append1d(
            expression,
            DotProduct2d(firstArgumentIndices, secondArgumentIndices, _)
          )
        }
        case Expression1d.SquaredNorm2d(argument) => {
          val (withArgument, argumentIndices) = evaluate(argument)
          withArgument.append1d(expression, SquaredNorm2d(argumentIndices, _))
        }
        case Expression1d.Sine(argument) => {
          val (withArgument, argumentIndex) = evaluate(argument)
          withArgument.append1d(expression, Sine(argumentIndex, _))
        }
        case Expression1d.Cosine(argument) => {
          val (withArgument, argumentIndex) = evaluate(argument)
          withArgument.append1d(expression, Cosine(argumentIndex, _))
        }
        case Expression1d.Tangent(argument) => {
          val (withArgument, argumentIndex) = evaluate(argument)
          withArgument.append1d(expression, Tangent(argumentIndex, _))
        }
        case Expression1d.Arcsine(argument) => {
          val (withArgument, argumentIndex) = evaluate(argument)
          withArgument.append1d(expression, Arcsine(argumentIndex, _))
        }
        case Expression1d.Arccosine(argument) => {
          val (withArgument, argumentIndex) = evaluate(argument)
          withArgument.append1d(expression, Arccosine(argumentIndex, _))
        }
        case Expression1d.Arctangent(argument) => {
          val (withArgument, argumentIndex) = evaluate(argument)
          withArgument.append1d(expression, Arctangent(argumentIndex, _))
        }
      }
    }

  def evaluate(expression: Expression2d[T]): (EvaluationSequence[T], (Int, Int)) =
    map2d.get(expression) match {
      case Some(indices) => (this, indices)
      case None => expression match {
        case Expression2d.Constant(x, y) =>
          append2d(expression, Constant2d(_, (x, y)))
        case Expression2d.Negation(argument) => {
          val (withArgument, argumentIndices) = evaluate(argument)
          withArgument.append2d(expression, Negation2d(argumentIndices, _))
        }
        case Expression2d.FromComponents(x, y) => {
          val (withX, xIndex) = this.evaluate(x)
          val (withY, yIndex) = withX.evaluate(y)
          (withY, (xIndex, yIndex))
        }
        case Expression2d.Sum(firstArgument, secondArgument) => {
          val (withFirst, firstArgumentIndices) = this.evaluate(firstArgument)
          val (withSecond, secondArgumentIndices) = withFirst.evaluate(secondArgument)
          withSecond.append2d(
            expression,
            Sum2d(firstArgumentIndices, secondArgumentIndices, _)
          )
        }
        case Expression2d.Difference(firstArgument, secondArgument) => {
          val (withFirst, firstArgumentIndices) = this.evaluate(firstArgument)
          val (withSecond, secondArgumentIndices) = withFirst.evaluate(secondArgument)
          withSecond.append2d(
            expression,
            Difference2d(firstArgumentIndices, secondArgumentIndices, _)
          )
        }
        case Expression2d.Product(firstArgument, secondArgument) => {
          val (withFirst, firstArgumentIndex) = this.evaluate(firstArgument)
          val (withSecond, secondArgumentIndices) = this.evaluate(secondArgument)
          withSecond.append2d(
            expression,
            Product2d(firstArgumentIndex, secondArgumentIndices, _)
          )
        }
        case Expression2d.Quotient(firstArgument, secondArgument) => {
          val (withFirst, firstArgumentIndices) = this.evaluate(firstArgument)
          val (withSecond, secondArgumentIndex) = this.evaluate(secondArgument)
          withSecond.append2d(
            expression,
            Quotient2d(firstArgumentIndices, secondArgumentIndex, _)
          )
        }
      }
    }

  private def append1d(
    expression: Expression1d[T],
    createOperation: (Int) => Operation
  ): (EvaluationSequence[T], Int) = {
    val resultIndex = arraySize
    val appendedSequence =
      EvaluationSequence[T](
        operations :+ createOperation(resultIndex),
        arraySize + 1,
        map1d + (expression -> resultIndex),
        map2d
      )
    (appendedSequence, resultIndex)
  }

  private def append2d(
    expression: Expression2d[T],
    createOperation: ((Int, Int)) => Operation
  ): (EvaluationSequence[T], (Int, Int)) = {
    val resultIndices = (arraySize, arraySize + 1)
    val appendedSequence =
      EvaluationSequence[T](
        operations :+ createOperation(resultIndices),
        arraySize + 2,
        map1d,
        map2d + (expression -> resultIndices)
      )
    (appendedSequence, resultIndices)
  }
}

object EvaluationSequence {
  def evaluate1d[T](expression: Expression1d[T]): (EvaluationSequence[T], Int) =
    empty1d[T].evaluate(expression)

  def evaluate2d[T](expression: Expression2d[T]): (EvaluationSequence[T], (Int, Int)) =
    empty2d[T].evaluate(expression)

  private def empty1d[T]: EvaluationSequence[T] =
    EvaluationSequence[T](List.empty, 1, Map.empty, Map.empty)

  private def empty2d[T]: EvaluationSequence[T] =
    EvaluationSequence[T](List.empty, 2, Map.empty, Map.empty)

  sealed abstract class Operation {
    def execute(values: Array[Double]): Unit

    def execute(values: Array[Interval]): Unit
  }

  case class Constant1d(resultIndex: Int, value: Double) extends Operation {
    private[this] val i = resultIndex;
    private[this] val interval = Interval.singleton(value)

    override def execute(values: Array[Double]): Unit = values(i) = value

    override def execute(values: Array[Interval]): Unit = values(i) = interval
  }

  case class Negation1d(argumentIndex: Int, resultIndex: Int) extends Operation {
    private[this] val i = resultIndex
    private[this] val j = argumentIndex

    override def execute(values: Array[Double]): Unit = values(i) = -values(j)

    override def execute(values: Array[Interval]): Unit = values(i) = -values(j)
  }

  case class Sum1d(firstArgumentIndex: Int, secondArgumentIndex: Int, resultIndex: Int)
    extends Operation {

    private[this] val i = resultIndex
    private[this] val j = firstArgumentIndex
    private[this] val k = secondArgumentIndex

    override def execute(values: Array[Double]): Unit = values(i) = values(j) + values(k)

    override def execute(values: Array[Interval]): Unit = values(i) = values(j) + values(k)
  }

  case class Difference1d(firstArgumentIndex: Int, secondArgumentIndex: Int, resultIndex: Int)
    extends Operation {

    private[this] val i = resultIndex
    private[this] val j = firstArgumentIndex
    private[this] val k = secondArgumentIndex

    override def execute(values: Array[Double]): Unit = values(i) = values(j) - values(k)

    override def execute(values: Array[Interval]): Unit = values(i) = values(j) - values(k)
  }

  case class Product1d(firstArgumentIndex: Int, secondArgumentIndex: Int, resultIndex: Int)
    extends Operation {

    private[this] val i = resultIndex
    private[this] val j = firstArgumentIndex
    private[this] val k = secondArgumentIndex

    override def execute(values: Array[Double]): Unit = values(i) = values(j) * values(k)

    override def execute(values: Array[Interval]): Unit = values(i) = values(j) * values(k)
  }

  case class Quotient1d(firstArgumentIndex: Int, secondArgumentIndex: Int, resultIndex: Int)
    extends Operation {

    private[this] val i = resultIndex
    private[this] val j = firstArgumentIndex
    private[this] val k = secondArgumentIndex

    override def execute(values: Array[Double]): Unit = values(i) = values(j) / values(k)

    override def execute(values: Array[Interval]): Unit = values(i) = values(j) / values(k)
  }

  case class Square(argumentIndex: Int, resultIndex: Int) extends Operation {
    private[this] val i = resultIndex
    private[this] val j = argumentIndex

    override def execute(values: Array[Double]): Unit = {
      val argument = values(j)
      values(i) = argument * argument
    }

    override def execute(values: Array[Interval]): Unit = values(i) = values(j).squared
  }

  case class SquareRoot(argumentIndex: Int, resultIndex: Int) extends Operation {
    private[this] val i = resultIndex
    private[this] val j = argumentIndex

    override def execute(values: Array[Double]): Unit = values(i) = math.sqrt(values(j))

    override def execute(values: Array[Interval]): Unit = values(i) = Interval.sqrt(values(j))
  }

  case class Sine(argumentIndex: Int, resultIndex: Int) extends Operation {
    private[this] val i = resultIndex
    private[this] val j = argumentIndex

    override def execute(values: Array[Double]): Unit = values(i) = math.sin(values(j))

    override def execute(values: Array[Interval]): Unit = values(i) = Interval.sin(values(j))
  }

  case class Cosine(argumentIndex: Int, resultIndex: Int) extends Operation {
    private[this] val i = resultIndex
    private[this] val j = argumentIndex

    override def execute(values: Array[Double]): Unit = values(i) = math.cos(values(j))

    override def execute(values: Array[Interval]): Unit = values(i) = Interval.cos(values(j))
  }

  case class Tangent(argumentIndex: Int, resultIndex: Int) extends Operation {
    private[this] val i = resultIndex
    private[this] val j = argumentIndex

    override def execute(values: Array[Double]): Unit = values(i) = math.tan(values(j))

    override def execute(values: Array[Interval]): Unit = values(i) = Interval.tan(values(j))
  }

  case class Arcsine(argumentIndex: Int, resultIndex: Int) extends Operation {
    private[this] val i = resultIndex
    private[this] val j = argumentIndex

    override def execute(values: Array[Double]): Unit = values(i) = math.asin(values(j))

    override def execute(values: Array[Interval]): Unit = values(i) = Interval.asin(values(j))
  }

  case class Arccosine(argumentIndex: Int, resultIndex: Int) extends Operation {
    private[this] val i = resultIndex
    private[this] val j = argumentIndex

    override def execute(values: Array[Double]): Unit = values(i) = math.acos(values(j))

    override def execute(values: Array[Interval]): Unit = values(i) = Interval.acos(values(j))
  }

  case class Arctangent(argumentIndex: Int, resultIndex: Int) extends Operation {
    private[this] val i = resultIndex
    private[this] val j = argumentIndex

    override def execute(values: Array[Double]): Unit = values(i) = math.atan(values(j))

    override def execute(values: Array[Interval]): Unit = values(i) = Interval.atan(values(j))
  }

  case class DotProduct2d(
    firstArgumentIndices: (Int, Int),
    secondArgumentIndices: (Int, Int),
    resultIndex: Int
  ) extends Operation {

    private[this] val i = resultIndex
    private[this] val (jx, jy) = firstArgumentIndices
    private[this] val (kx, ky) = secondArgumentIndices

    override def execute(values: Array[Double]): Unit =
      values(i) = values(jx) * values(kx) + values(jy) * values(ky)

    override def execute(values: Array[Interval]): Unit =
      values(i) = values(jx) * values(kx) + values(jy) * values(ky)
  }

  case class SquaredNorm2d(argumentIndices: (Int, Int), resultIndex: Int) extends Operation {
    private[this] val i = resultIndex
    private[this] val (jx, jy) = argumentIndices

    override def execute(values: Array[Double]): Unit = {
      val x = values(jx)
      val y = values(jy)
      values(i) = x * x + y * y
    }

    override def execute(values: Array[Interval]): Unit = {
      values(i) = values(jx).squared + values(jy).squared
    }
  }

  case class Constant2d(resultIndices: (Int, Int), values: (Double, Double)) extends Operation {
    private[this] val (ix, iy) = resultIndices;
    private[this] val (x, y) = values
    private[this] val xInterval = Interval.singleton(x)
    private[this] val yInterval = Interval.singleton(y)

    override def execute(values: Array[Double]): Unit = {
      values(ix) = x
      values(iy) = y
    }

    override def execute(values: Array[Interval]): Unit = {
      values(ix) = xInterval
      values(iy) = yInterval
    }
  }

  case class Negation2d(argumentIndices: (Int, Int), resultIndices: (Int, Int)) extends Operation {
    private[this] val (ix, iy) = resultIndices
    private[this] val (jx, jy) = argumentIndices

    override def execute(values: Array[Double]): Unit = {
      values(ix) = -values(jx)
      values(iy) = -values(jy)
    }

    override def execute(values: Array[Interval]): Unit = {
      values(ix) = -values(jx)
      values(iy) = -values(jy)
    }
  }

  case class Sum2d(
    firstArgumentIndices: (Int, Int),
    secondArgumentIndices: (Int, Int),
    resultIndices: (Int, Int)
  ) extends Operation {

    private[this] val (ix, iy) = resultIndices
    private[this] val (jx, jy) = firstArgumentIndices
    private[this] val (kx, ky) = secondArgumentIndices

    override def execute(values: Array[Double]): Unit = {
      values(ix) = values(jx) + values(kx)
      values(iy) = values(jy) + values(ky)
    }

    override def execute(values: Array[Interval]): Unit = {
      values(ix) = values(jx) + values(kx)
      values(iy) = values(jy) + values(ky)
    }
  }

  case class Difference2d(
    firstArgumentIndices: (Int, Int),
    secondArgumentIndices: (Int, Int),
    resultIndices: (Int, Int)
  ) extends Operation {

    private[this] val (ix, iy) = resultIndices
    private[this] val (jx, jy) = firstArgumentIndices
    private[this] val (kx, ky) = secondArgumentIndices

    override def execute(values: Array[Double]): Unit = {
      values(ix) = values(jx) - values(kx)
      values(iy) = values(jy) - values(ky)
    }

    override def execute(values: Array[Interval]): Unit = {
      values(ix) = values(jx) - values(kx)
      values(iy) = values(jy) - values(ky)
    }
  }

  case class Product2d(
    firstArgumentIndex: Int,
    secondArgumentIndices: (Int, Int),
    resultIndices: (Int, Int)
  ) extends Operation {

    private[this] val (ix, iy) = resultIndices
    private[this] val j = firstArgumentIndex
    private[this] val (kx, ky) = secondArgumentIndices

    override def execute(values: Array[Double]): Unit = {
      val multiplier = values(j)
      values(ix) = multiplier * values(kx)
      values(iy) = multiplier * values(ky)
    }

    override def execute(values: Array[Interval]): Unit = {
      val multiplier = values(j)
      values(ix) = multiplier * values(kx)
      values(iy) = multiplier * values(ky)
    }
  }

  case class Quotient2d(
    firstArgumentIndices: (Int, Int),
    secondArgumentIndex: Int,
    resultIndices: (Int, Int)
  ) extends Operation {

    private[this] val (ix, iy) = resultIndices
    private[this] val (jx, jy) = firstArgumentIndices
    private[this] val k = secondArgumentIndex

    override def execute(values: Array[Double]): Unit = {
      val divisor = values(k)
      values(ix) = values(jx) / divisor
      values(iy) = values(jy) / divisor
    }

    override def execute(values: Array[Interval]): Unit = {
      val divisor = values(k)
      values(ix) = values(jx) / divisor
      values(iy) = values(jy) / divisor
    }
  }
}
