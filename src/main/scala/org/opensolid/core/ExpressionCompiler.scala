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

import scala.collection.mutable

private class ExpressionCompiler[T] {
  import ExpressionCompiler._

  val arrayOperations: mutable.ArrayBuffer[ArrayOperation] =
    new mutable.ArrayBuffer[ArrayOperation]

  var arraySize: Int = 0

  private[this] val expressionMap1d: mutable.Map[Expression1d[T], Int] =
    mutable.Map.empty[Expression1d[T], Int]

  private[this] val expressionMap2d: mutable.Map[Expression2d[T], (Int, Int)] =
    mutable.Map.empty[Expression2d[T], (Int, Int)]

  def evaluate(expression: Expression1d[T]): Int =
    expressionMap1d.get(expression) match {
      case Some(index) => index
      case None => expression match {
        case parameter: Expression1d.Parameter[T] =>
          parameter.index
        case Expression1d.Constant(value) =>
          add1d(expression, new Constant1d(value, _))
        case Expression1d.XComponent2d(expression) => {
          val (xIndex, yIndex) = evaluate(expression)
          xIndex
        }
        case Expression1d.YComponent2d(expression) => {
          val (xIndex, yIndex) = evaluate(expression)
          yIndex
        }
        case Expression1d.Negation(argument) => {
          val argumentIndex = evaluate(argument)
          add1d(expression, new Negation1d(argumentIndex, _))
        }
        case Expression1d.Sum(firstArgument, secondArgument) => {
          val firstArgumentIndex = evaluate(firstArgument)
          val secondArgumentIndex = evaluate(secondArgument)
          add1d(expression, new Sum1d(firstArgumentIndex, secondArgumentIndex, _))
        }
        case Expression1d.Difference(firstArgument, secondArgument) => {
          val firstArgumentIndex = evaluate(firstArgument)
          val secondArgumentIndex = evaluate(secondArgument)
          add1d(expression, new Difference1d(firstArgumentIndex, secondArgumentIndex, _))
        }
        case Expression1d.Product(firstArgument, secondArgument) => {
          val firstArgumentIndex = evaluate(firstArgument)
          val secondArgumentIndex = evaluate(secondArgument)
          add1d(expression, new Product1d(firstArgumentIndex, secondArgumentIndex, _))
        }
        case Expression1d.Quotient(firstArgument, secondArgument) => {
          val firstArgumentIndex = evaluate(firstArgument)
          val secondArgumentIndex = evaluate(secondArgument)
          add1d(expression, new Quotient1d(firstArgumentIndex, secondArgumentIndex, _))
        }
        case Expression1d.Square(argument) => {
          val argumentIndex = evaluate(argument)
          add1d(expression, new Square(argumentIndex, _))
        }
        case Expression1d.SquareRoot(argument) => {
          val argumentIndex = evaluate(argument)
          add1d(expression, new SquareRoot(argumentIndex, _))
        }
        case Expression1d.DotProduct2d(firstArgument, secondArgument) => {
          val firstArgumentIndices = evaluate(firstArgument)
          val secondArgumentIndices = evaluate(secondArgument)
          add1d(expression, new DotProduct2d(firstArgumentIndices, secondArgumentIndices, _))
        }
        case Expression1d.SquaredNorm2d(argument) => {
          val argumentIndices = evaluate(argument)
          add1d(expression, new SquaredNorm2d(argumentIndices, _))
        }
        case Expression1d.Sine(argument) => {
          val argumentIndex = evaluate(argument)
          add1d(expression, new Sine(argumentIndex, _))
        }
        case Expression1d.Cosine(argument) => {
          val argumentIndex = evaluate(argument)
          add1d(expression, new Cosine(argumentIndex, _))
        }
        case Expression1d.Tangent(argument) => {
          val argumentIndex = evaluate(argument)
          add1d(expression, new Tangent(argumentIndex, _))
        }
        case Expression1d.Arcsine(argument) => {
          val argumentIndex = evaluate(argument)
          add1d(expression, new Arcsine(argumentIndex, _))
        }
        case Expression1d.Arccosine(argument) => {
          val argumentIndex = evaluate(argument)
          add1d(expression, new Arccosine(argumentIndex, _))
        }
        case Expression1d.Arctangent(argument) => {
          val argumentIndex = evaluate(argument)
          add1d(expression, new Arctangent(argumentIndex, _))
        }
      }
    }

  def evaluate(expression: Expression2d[T]): (Int, Int) =
    expressionMap2d.get(expression) match {
      case Some(indices) => indices
      case None => expression match {
        case Expression2d.Constant(x, y) =>
          add2d(expression, new Constant2d((x, y), _))
        case Expression2d.FromComponents(x, y) => {
          val xIndex = evaluate(x)
          val yIndex = evaluate(y)
          (xIndex, yIndex)
        }
        case Expression2d.Negation(argument) => {
          val argumentIndices = evaluate(argument)
          add2d(expression, new Negation2d(argumentIndices, _))
        }
        case Expression2d.Sum(firstArgument, secondArgument) => {
          val firstArgumentIndices = evaluate(firstArgument)
          val secondArgumentIndices = evaluate(secondArgument)
          add2d(expression, new Sum2d(firstArgumentIndices, secondArgumentIndices, _))
        }
        case Expression2d.Difference(firstArgument, secondArgument) => {
          val firstArgumentIndices = evaluate(firstArgument)
          val secondArgumentIndices = evaluate(secondArgument)
          add2d(expression, new Difference2d(firstArgumentIndices, secondArgumentIndices, _))
        }
        case Expression2d.Product(firstArgument, secondArgument) => {
          val firstArgumentIndex = evaluate(firstArgument)
          val secondArgumentIndices = evaluate(secondArgument)
          add2d(expression, new Product2d(firstArgumentIndex, secondArgumentIndices, _))
        }
        case Expression2d.Quotient(firstArgument, secondArgument) => {
          val firstArgumentIndices = evaluate(firstArgument)
          val secondArgumentIndex = evaluate(secondArgument)
          add2d(expression, new Quotient2d(firstArgumentIndices, secondArgumentIndex, _))
        }
      }
    }

  private[this] def add1d(
    expression: Expression1d[T],
    createOperation: (Int) => ArrayOperation
  ): Int = {
    val resultIndex = arraySize
    arrayOperations += createOperation(resultIndex)
    arraySize += 1
    expressionMap1d += (expression -> resultIndex)
    resultIndex
  }

  private[this] def add2d(
    expression: Expression2d[T],
    createOperation: ((Int, Int)) => ArrayOperation
  ): (Int, Int) = {
    val resultIndices = (arraySize, arraySize + 1)
    arrayOperations += createOperation(resultIndices)
    arraySize += 2
    expressionMap2d += (expression -> resultIndices)
    resultIndices
  }
}

object ExpressionCompiler {
  def compile[T](expression: Expression1d[T]): (Array[ArrayOperation], Int, Int) = {
    val compiler = new ExpressionCompiler[T]
    val resultIndex = compiler.evaluate(expression)
    (compiler.arrayOperations.toArray, compiler.arraySize, resultIndex)
  }

  def compile[T](expression: Expression2d[T]): (Array[ArrayOperation], Int, (Int, Int)) = {
    val compiler = new ExpressionCompiler[T]
    val resultIndices = compiler.evaluate(expression)
    (compiler.arrayOperations.toArray, compiler.arraySize, resultIndices)
  }

  sealed abstract class ArrayOperation {
    def execute(values: Array[Double]): Unit

    def execute(values: Array[Interval]): Unit
  }

  private[ExpressionCompiler] class Constant1d(value: Double, resultIndex: Int)
    extends ArrayOperation {

    private[this] val interval = Interval.singleton(value)

    override def execute(values: Array[Double]): Unit = values(resultIndex) = value

    override def execute(values: Array[Interval]): Unit = values(resultIndex) = interval
  }

  private[ExpressionCompiler] class Negation1d(argumentIndex: Int, resultIndex: Int)
    extends ArrayOperation {

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) = -values(argumentIndex)

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = -values(argumentIndex)
  }

  private[ExpressionCompiler] class Sum1d(
    firstArgumentIndex: Int,
    secondArgumentIndex: Int,
    resultIndex: Int
  ) extends ArrayOperation {

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) = values(firstArgumentIndex) + values(secondArgumentIndex)

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = values(firstArgumentIndex) + values(secondArgumentIndex)
  }

  private[ExpressionCompiler] class Difference1d(
    firstArgumentIndex: Int,
    secondArgumentIndex: Int,
    resultIndex: Int
  ) extends ArrayOperation {

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) = values(firstArgumentIndex) - values(secondArgumentIndex)

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = values(firstArgumentIndex) - values(secondArgumentIndex)
  }

  private[ExpressionCompiler] class Product1d(
    firstArgumentIndex: Int,
    secondArgumentIndex: Int,
    resultIndex: Int
  ) extends ArrayOperation {

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) = values(firstArgumentIndex) * values(secondArgumentIndex)

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = values(firstArgumentIndex) * values(secondArgumentIndex)
  }

  private[ExpressionCompiler] class Quotient1d(
    firstArgumentIndex: Int,
    secondArgumentIndex: Int,
    resultIndex: Int
  ) extends ArrayOperation {

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) = values(firstArgumentIndex) / values(secondArgumentIndex)

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = values(firstArgumentIndex) / values(secondArgumentIndex)
  }

  private[ExpressionCompiler] class Square(argumentIndex: Int, resultIndex: Int)
    extends ArrayOperation {

    override def execute(values: Array[Double]): Unit = {
      val argument = values(argumentIndex)
      values(resultIndex) = argument * argument
    }

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = values(argumentIndex).squared
  }

  private[ExpressionCompiler] class SquareRoot(argumentIndex: Int, resultIndex: Int)
    extends ArrayOperation {

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) = math.sqrt(values(argumentIndex))

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = Interval.sqrt(values(argumentIndex))
  }

  private[ExpressionCompiler] class Sine(argumentIndex: Int, resultIndex: Int)
    extends ArrayOperation {

    override def execute(values: Array[Double]): Unit = values(resultIndex) =
      math.sin(values(argumentIndex))

    override def execute(values: Array[Interval]): Unit = values(resultIndex) =
      Interval.sin(values(argumentIndex))
  }

  private[ExpressionCompiler] class Cosine(argumentIndex: Int, resultIndex: Int)
    extends ArrayOperation {

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) = math.cos(values(argumentIndex))

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = Interval.cos(values(argumentIndex))
  }

  private[ExpressionCompiler] class Tangent(argumentIndex: Int, resultIndex: Int)
    extends ArrayOperation {

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) = math.tan(values(argumentIndex))

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = Interval.tan(values(argumentIndex))
  }

  private[ExpressionCompiler] class Arcsine(argumentIndex: Int, resultIndex: Int)
    extends ArrayOperation {

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) = math.asin(values(argumentIndex))

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = Interval.asin(values(argumentIndex))
  }

  private[ExpressionCompiler] class Arccosine(argumentIndex: Int, resultIndex: Int)
    extends ArrayOperation {

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) = math.acos(values(argumentIndex))

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = Interval.acos(values(argumentIndex))
  }

  private[ExpressionCompiler] class Arctangent(argumentIndex: Int, resultIndex: Int)
    extends ArrayOperation {

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) = math.atan(values(argumentIndex))

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = Interval.atan(values(argumentIndex))
  }

  private[ExpressionCompiler] class DotProduct2d(
    firstArgumentIndices: (Int, Int),
    secondArgumentIndices: (Int, Int),
    resultIndex: Int
  ) extends ArrayOperation {

    private[this] val (firstArgumentXIndex, firstArgumentYIndex) = firstArgumentIndices
    private[this] val (secondArgumentXIndex, secondArgumentYIndex) = secondArgumentIndices

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) =
        values(firstArgumentXIndex) * values(secondArgumentXIndex) +
        values(firstArgumentYIndex) * values(secondArgumentYIndex)

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) =
        values(firstArgumentXIndex) * values(secondArgumentXIndex) +
        values(firstArgumentYIndex) * values(secondArgumentYIndex)
  }

  private[ExpressionCompiler] class SquaredNorm2d(argumentIndices: (Int, Int), resultIndex: Int)
    extends ArrayOperation {

    private[this] val (argumentXIndex, argumentYIndex) = argumentIndices

    override def execute(values: Array[Double]): Unit = {
      val argumentX = values(argumentXIndex)
      val argumentY = values(argumentYIndex)
      values(resultIndex) = argumentX * argumentX + argumentY * argumentY
    }

    override def execute(values: Array[Interval]): Unit = {
      values(resultIndex) = values(argumentXIndex).squared + values(argumentYIndex).squared
    }
  }

  private[ExpressionCompiler] class Constant2d(values: (Double, Double), resultIndices: (Int, Int))
    extends ArrayOperation {

    private[this] val (xValue, yValue) = values
    private[this] val xInterval = Interval.singleton(xValue)
    private[this] val yInterval = Interval.singleton(yValue)
    private[this] val (resultXIndex, resultYIndex) = resultIndices

    override def execute(values: Array[Double]): Unit = {
      values(resultXIndex) = xValue
      values(resultYIndex) = yValue
    }

    override def execute(values: Array[Interval]): Unit = {
      values(resultXIndex) = xInterval
      values(resultYIndex) = yInterval
    }
  }

  private[ExpressionCompiler] class Negation2d(
    argumentIndices: (Int, Int),
    resultIndices: (Int, Int)
  ) extends ArrayOperation {

    private[this] val (argumentXIndex, argumentYIndex) = argumentIndices
    private[this] val (resultXIndex, resultYIndex) = resultIndices

    override def execute(values: Array[Double]): Unit = {
      values(resultXIndex) = -values(argumentXIndex)
      values(resultYIndex) = -values(argumentYIndex)
    }

    override def execute(values: Array[Interval]): Unit = {
      values(resultXIndex) = -values(argumentXIndex)
      values(resultYIndex) = -values(argumentYIndex)
    }
  }

  private[ExpressionCompiler] class Sum2d(
    firstArgumentIndices: (Int, Int),
    secondArgumentIndices: (Int, Int),
    resultIndices: (Int, Int)
  ) extends ArrayOperation {

    private[this] val (firstArgumentXIndex, firstArgumentYIndex) = firstArgumentIndices
    private[this] val (secondArgumentXIndex, secondArgumentYIndex) = secondArgumentIndices
    private[this] val (resultXIndex, resultYIndex) = resultIndices

    override def execute(values: Array[Double]): Unit = {
      values(resultXIndex) = values(firstArgumentXIndex) + values(secondArgumentXIndex)
      values(resultYIndex) = values(firstArgumentYIndex) + values(secondArgumentYIndex)
    }

    override def execute(values: Array[Interval]): Unit = {
      values(resultXIndex) = values(firstArgumentXIndex) + values(secondArgumentXIndex)
      values(resultYIndex) = values(firstArgumentYIndex) + values(secondArgumentYIndex)
    }
  }

  private[ExpressionCompiler] class Difference2d(
    firstArgumentIndices: (Int, Int),
    secondArgumentIndices: (Int, Int),
    resultIndices: (Int, Int)
  ) extends ArrayOperation {

    private[this] val (firstArgumentXIndex, firstArgumentYIndex) = firstArgumentIndices
    private[this] val (secondArgumentXIndex, secondArgumentYIndex) = secondArgumentIndices
    private[this] val (resultXIndex, resultYIndex) = resultIndices

    override def execute(values: Array[Double]): Unit = {
      values(resultXIndex) = values(firstArgumentXIndex) - values(secondArgumentXIndex)
      values(resultYIndex) = values(firstArgumentYIndex) - values(secondArgumentYIndex)
    }

    override def execute(values: Array[Interval]): Unit = {
      values(resultXIndex) = values(firstArgumentXIndex) - values(secondArgumentXIndex)
      values(resultYIndex) = values(firstArgumentYIndex) - values(secondArgumentYIndex)
    }
  }

  private[ExpressionCompiler] class Product2d(
    firstArgumentIndex: Int,
    secondArgumentIndices: (Int, Int),
    resultIndices: (Int, Int)
  ) extends ArrayOperation {

    private[this] val (secondArgumentXIndex, secondArgumentYIndex) = secondArgumentIndices
    private[this] val (resultXIndex, resultYIndex) = resultIndices

    override def execute(values: Array[Double]): Unit = {
      val multiplier = values(firstArgumentIndex)
      values(resultXIndex) = multiplier * values(secondArgumentXIndex)
      values(resultYIndex) = multiplier * values(secondArgumentYIndex)
    }

    override def execute(values: Array[Interval]): Unit = {
      val multiplier = values(firstArgumentIndex)
      values(resultXIndex) = multiplier * values(secondArgumentXIndex)
      values(resultYIndex) = multiplier * values(secondArgumentYIndex)
    }
  }

  private[ExpressionCompiler] class Quotient2d(
    firstArgumentIndices: (Int, Int),
    secondArgumentIndex: Int,
    resultIndices: (Int, Int)
  ) extends ArrayOperation {

    private[this] val (firstArgumentXIndex, firstArgumentYIndex) = firstArgumentIndices
    private[this] val (resultXIndex, resultYIndex) = resultIndices

    override def execute(values: Array[Double]): Unit = {
      val divisor = values(secondArgumentIndex)
      values(resultXIndex) = values(firstArgumentXIndex) / divisor
      values(resultYIndex) = values(firstArgumentYIndex) / divisor
    }

    override def execute(values: Array[Interval]): Unit = {
      val divisor = values(secondArgumentIndex)
      values(resultXIndex) = values(firstArgumentXIndex) / divisor
      values(resultYIndex) = values(firstArgumentYIndex) / divisor
    }
  }
}
