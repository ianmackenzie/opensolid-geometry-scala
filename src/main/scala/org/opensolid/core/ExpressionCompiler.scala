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

private class ExpressionCompiler {
  import ExpressionCompiler._

  val arrayOperations: mutable.ArrayBuffer[ArrayOperation] =
    new mutable.ArrayBuffer[ArrayOperation]

  var arraySize: Int = 0

  private[this] val constantMap = mutable.Map.empty[Double, Int]
  private[this] val negation1dMap = mutable.Map.empty[Int, Int]
  private[this] val sum1dMap = mutable.Map.empty[(Int, Int), Int]
  private[this] val difference1dMap = mutable.Map.empty[(Int, Int), Int]
  private[this] val product1dMap = mutable.Map.empty[(Int, Int), Int]
  private[this] val quotient1dMap = mutable.Map.empty[(Int, Int), Int]
  private[this] val squareMap = mutable.Map.empty[Int, Int]
  private[this] val squareRootMap = mutable.Map.empty[Int, Int]
  private[this] val sineMap = mutable.Map.empty[Int, Int]
  private[this] val cosineMap = mutable.Map.empty[Int, Int]
  private[this] val tangentMap = mutable.Map.empty[Int, Int]
  private[this] val arcsineMap = mutable.Map.empty[Int, Int]
  private[this] val arccosineMap = mutable.Map.empty[Int, Int]
  private[this] val arctangentMap = mutable.Map.empty[Int, Int]
  private[this] val dotProduct2dMap = mutable.Map.empty[((Int, Int), (Int, Int)), Int]
  private[this] val crossProduct2dMap = mutable.Map.empty[((Int, Int), (Int, Int)), Int]
  private[this] val squaredNorm2dMap = mutable.Map.empty[(Int, Int), Int]
  private[this] val negation2dMap = mutable.Map.empty[(Int, Int), (Int, Int)]
  private[this] val sum2dMap = mutable.Map.empty[((Int, Int), (Int, Int)), (Int, Int)]
  private[this] val difference2dMap = mutable.Map.empty[((Int, Int), (Int, Int)), (Int, Int)]
  private[this] val product2dMap = mutable.Map.empty[(Int, (Int, Int)), (Int, Int)]
  private[this] val quotient2dMap = mutable.Map.empty[((Int, Int), Int), (Int, Int)]

  def evaluate(expression: ScalarExpression[_]): Int = expression match {
    case ScalarExpression.Parameter(index) =>
      index
    case ScalarExpression.Constant(value) =>
      constant1d(value)
    case ScalarExpression.VectorXComponent2d(expression) =>
      evaluate(expression).first
    case ScalarExpression.VectorYComponent2d(expression) =>
      evaluate(expression).second
    case ScalarExpression.Negation(argument) =>
      negation1d(evaluate(argument))
    case ScalarExpression.Sum(firstArgument, secondArgument) =>
      sum1d(evaluate(firstArgument), evaluate(secondArgument))
    case ScalarExpression.Difference(firstArgument, secondArgument) =>
      difference1d(evaluate(firstArgument), evaluate(secondArgument))
    case ScalarExpression.Product(firstArgument, secondArgument) =>
      product1d(evaluate(firstArgument), evaluate(secondArgument))
    case ScalarExpression.Quotient(firstArgument, secondArgument) =>
      quotient1d(evaluate(firstArgument), evaluate(secondArgument))
    case ScalarExpression.Square(argument) =>
      square(evaluate(argument))
    case ScalarExpression.SquareRoot(argument) =>
      squareRoot(evaluate(argument))
    case ScalarExpression.Sine(argument) =>
      sine(evaluate(argument))
    case ScalarExpression.Cosine(argument) =>
      cosine(evaluate(argument))
    case ScalarExpression.Tangent(argument) =>
      tangent(evaluate(argument))
    case ScalarExpression.Arcsine(argument) =>
      arcsine(evaluate(argument))
    case ScalarExpression.Arccosine(argument) =>
      arccosine(evaluate(argument))
    case ScalarExpression.Arctangent(argument) =>
      arctangent(evaluate(argument))
    case ScalarExpression.DotProduct2d(firstArgument, secondArgument) =>
      dotProduct2d(evaluate(firstArgument), evaluate(secondArgument))
    case ScalarExpression.SquaredLength2d(argument) =>
      squaredNorm2d(evaluate(argument))
  }

  def evaluate(expression: VectorExpression2d[_]): (Int, Int) = expression match {
    case VectorExpression2d.Constant(vector) =>
      constant2d(vector.x, vector.y)
    case VectorExpression2d.FromComponents(x, y) =>
      (evaluate(x), evaluate(y))
    case VectorExpression2d.Negation(argument) =>
      negation2d(evaluate(argument))
    case VectorExpression2d.Sum(firstArgument, secondArgument) =>
      sum2d(evaluate(firstArgument), evaluate(secondArgument))
    case VectorExpression2d.Difference(firstArgument, secondArgument) =>
      difference2d(evaluate(firstArgument), evaluate(secondArgument))
    case VectorExpression2d.Product(firstArgument, secondArgument) =>
      product2d(evaluate(firstArgument), evaluate(secondArgument))
    case VectorExpression2d.Quotient(firstArgument, secondArgument) =>
      quotient2d(evaluate(firstArgument), evaluate(secondArgument))
  }

  private[this] def constant1d(value: Double): Int = constantMap.get(value) match {
    case Some(index) => index
    case None => {
      val resultIndex = arraySize
      arraySize += 1
      arrayOperations += new Constant1d(value, resultIndex)
      constantMap += (value -> resultIndex)
      resultIndex
    }
  }

  private[this] def unaryOperation1d(
    argumentIndex: Int,
    resultMap: mutable.Map[Int, Int],
    newOperation: (Int) => ArrayOperation
  ): Int = resultMap.get(argumentIndex) match {
    case Some(index) => index
    case None => {
      val resultIndex = arraySize
      arraySize += 1
      arrayOperations += newOperation(resultIndex)
      resultMap += (argumentIndex -> resultIndex)
      resultIndex
    }
  }

  private[this] def binaryOperation1d(
    firstArgumentIndex: Int,
    secondArgumentIndex: Int,
    resultMap: mutable.Map[(Int, Int), Int],
    newOperation: (Int) => ArrayOperation,
    symmetric: Boolean = false
  ): Int = {
    val argumentIndices = (firstArgumentIndex, secondArgumentIndex)
    resultMap.get(argumentIndices) match {
      case Some(index) => index
      case None => {
        val resultIndex = arraySize
        arraySize += 1
        arrayOperations += newOperation(resultIndex)
        resultMap += (argumentIndices -> resultIndex)
        if (symmetric) resultMap += (argumentIndices.reverse -> resultIndex)
        resultIndex
      }
    }
  }

  private[this] def unaryOperation2d(
    argumentIndices: (Int, Int),
    resultMap: mutable.Map[(Int, Int), (Int, Int)],
    newOperation: ((Int, Int)) => ArrayOperation
  ): (Int, Int) = resultMap.get(argumentIndices) match {
    case Some(indices) => indices
    case None => {
      val resultIndices = (arraySize, arraySize + 1)
      arraySize += 2
      arrayOperations += newOperation(resultIndices)
      resultMap += (argumentIndices -> resultIndices)
      resultIndices
    }
  }

  private[this] def binaryOperation2d(
    firstArgumentIndices: (Int, Int),
    secondArgumentIndices: (Int, Int),
    resultMap: mutable.Map[((Int, Int), (Int, Int)), (Int, Int)],
    newOperation: ((Int, Int)) => ArrayOperation,
    symmetric: Boolean = false
  ): (Int, Int) = resultMap.get((firstArgumentIndices, secondArgumentIndices)) match {
    case Some(indices) => indices
    case None => {
      val resultIndices = (arraySize, arraySize + 1)
      arraySize += 2
      arrayOperations += newOperation(resultIndices)
      resultMap += ((firstArgumentIndices, secondArgumentIndices) -> resultIndices)
      if (symmetric) resultMap += ((secondArgumentIndices, firstArgumentIndices) -> resultIndices)
      resultIndices
    }
  }

  private[this] def negation1d(argumentIndex: Int): Int =
    unaryOperation1d(argumentIndex, negation1dMap, new Negation1d(argumentIndex, _))

  private[this] def sum1d(firstArgumentIndex: Int, secondArgumentIndex: Int): Int =
    binaryOperation1d(
      firstArgumentIndex,
      secondArgumentIndex,
      sum1dMap,
      new Sum1d(firstArgumentIndex, secondArgumentIndex, _),
      true
    )

  private[this] def difference1d(firstArgumentIndex: Int, secondArgumentIndex: Int): Int =
    binaryOperation1d(
      firstArgumentIndex,
      secondArgumentIndex,
      difference1dMap,
      new Difference1d(firstArgumentIndex, secondArgumentIndex, _)
    )

  private[this] def product1d(firstArgumentIndex: Int, secondArgumentIndex: Int): Int =
    binaryOperation1d(
      firstArgumentIndex,
      secondArgumentIndex,
      product1dMap,
      new Product1d(firstArgumentIndex, secondArgumentIndex, _),
      true
    )

  private[this] def quotient1d(firstArgumentIndex: Int, secondArgumentIndex: Int): Int =
    binaryOperation1d(
      firstArgumentIndex,
      secondArgumentIndex,
      quotient1dMap,
      new Quotient1d(firstArgumentIndex, secondArgumentIndex, _)
    )

  private[this] def square(argumentIndex: Int): Int =
    unaryOperation1d(argumentIndex, squareMap, new Square(argumentIndex, _))

  private[this] def squareRoot(argumentIndex: Int): Int =
    unaryOperation1d(argumentIndex, squareRootMap, new SquareRoot(argumentIndex, _))

  private[this] def sine(argumentIndex: Int): Int =
    unaryOperation1d(argumentIndex, sineMap, new Sine(argumentIndex, _))

  private[this] def cosine(argumentIndex: Int): Int =
    unaryOperation1d(argumentIndex, cosineMap, new Cosine(argumentIndex, _))

  private[this] def tangent(argumentIndex: Int): Int =
    unaryOperation1d(argumentIndex, tangentMap, new Tangent(argumentIndex, _))

  private[this] def arcsine(argumentIndex: Int): Int =
    unaryOperation1d(argumentIndex, arcsineMap, new Arcsine(argumentIndex, _))

  private[this] def arccosine(argumentIndex: Int): Int =
    unaryOperation1d(argumentIndex, arccosineMap, new Arccosine(argumentIndex, _))

  private[this] def arctangent(argumentIndex: Int): Int =
    unaryOperation1d(argumentIndex, arctangentMap, new Arctangent(argumentIndex, _))

  private[this] def dotProduct2d(
    firstArgumentIndices: (Int, Int),
    secondArgumentIndices: (Int, Int)
  ): Int = dotProduct2dMap.get((firstArgumentIndices, secondArgumentIndices)) match {
    case Some(index) => index
    case None => {
      val resultIndex = arraySize
      arraySize += 1
      arrayOperations += new DotProduct2d(firstArgumentIndices, secondArgumentIndices, resultIndex)
      dotProduct2dMap += ((firstArgumentIndices, secondArgumentIndices) -> resultIndex)
      dotProduct2dMap += ((secondArgumentIndices, firstArgumentIndices) -> resultIndex)
      resultIndex
    }
  }

  private[this] def squaredNorm2d(argumentIndices: (Int, Int)): Int =
    binaryOperation1d(
      argumentIndices.first,
      argumentIndices.second,
      squaredNorm2dMap,
      new SquaredNorm2d(argumentIndices, _),
      true
    )

  private[this] def constant2d(xValue: Double, yValue: Double): (Int, Int) =
    (constantMap.get(xValue), constantMap.get(yValue)) match {
      case (Some(xIndex), Some(yIndex)) =>
        (xIndex, yIndex)
      case (Some(xIndex), None) => {
        val yIndex = arraySize
        arraySize += 1
        arrayOperations += new Constant1d(yValue, yIndex)
        constantMap += (yValue -> yIndex)
        (xIndex, yIndex)
      }
      case (None, Some(yIndex)) => {
        val xIndex = arraySize
        arraySize += 1
        arrayOperations += new Constant1d(xValue, xIndex)
        constantMap += (xValue -> xIndex)
        (xIndex, yIndex)
      }
      case (None, None) => {
        val resultIndices = (arraySize, arraySize + 1)
        arraySize += 2
        arrayOperations += new Constant2d((xValue, yValue), resultIndices)
        constantMap += (xValue -> resultIndices.first)
        constantMap += (yValue -> resultIndices.second)
        resultIndices
      }
    }

  private[this] def negation2d(argumentIndices: (Int, Int)): (Int, Int) =
    unaryOperation2d(argumentIndices, negation2dMap, new Negation2d(argumentIndices, _))

  private[this] def sum2d(
    firstArgumentIndices: (Int, Int),
    secondArgumentIndices: (Int, Int)
  ): (Int, Int) =
    binaryOperation2d(
      firstArgumentIndices,
      secondArgumentIndices,
      sum2dMap,
      new Sum2d(firstArgumentIndices, secondArgumentIndices, _),
      true
    )

  private[this] def difference2d(
    firstArgumentIndices: (Int, Int),
    secondArgumentIndices: (Int, Int)
  ): (Int, Int) =
    binaryOperation2d(
      firstArgumentIndices,
      secondArgumentIndices,
      difference2dMap,
      new Difference2d(firstArgumentIndices, secondArgumentIndices, _)
    )

  private[this] def product2d(
    firstArgumentIndex: Int,
    secondArgumentIndices: (Int, Int)
  ): (Int, Int) = product2dMap.get((firstArgumentIndex, secondArgumentIndices)) match {
    case Some(indices) => indices
    case None => {
      val resultIndices = (arraySize, arraySize + 1)
      arraySize += 2
      arrayOperations += new Product2d(firstArgumentIndex, secondArgumentIndices, resultIndices)
      product2dMap += ((firstArgumentIndex, secondArgumentIndices) -> resultIndices)
      resultIndices
    }
  }

  private[this] def quotient2d(
    firstArgumentIndices: (Int, Int),
    secondArgumentIndex: Int
  ): (Int, Int) = quotient2dMap.get((firstArgumentIndices, secondArgumentIndex)) match {
    case Some(indices) => indices
    case None => {
      val resultIndices = (arraySize, arraySize + 1)
      arraySize += 2
      arrayOperations += new Quotient2d(firstArgumentIndices, secondArgumentIndex, resultIndices)
      quotient2dMap += ((firstArgumentIndices, secondArgumentIndex) -> resultIndices)
      resultIndices
    }
  }
}

object ExpressionCompiler {
  def compile(expression: ScalarExpression[_]): (Array[ArrayOperation], Int, Int) = {
    val compiler = new ExpressionCompiler
    val resultIndex = compiler.evaluate(expression)
    (compiler.arrayOperations.toArray, compiler.arraySize, resultIndex)
  }

  def compile(expression: VectorExpression2d[_]): (Array[ArrayOperation], Int, (Int, Int)) = {
    val compiler = new ExpressionCompiler
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

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) = value

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = interval
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

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) = math.sin(values(argumentIndex))

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = Interval.sin(values(argumentIndex))
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

  private[ExpressionCompiler] class CrossProduct2d(
    firstArgumentIndices: (Int, Int),
    secondArgumentIndices: (Int, Int),
    resultIndex: Int
  ) extends ArrayOperation {

    private[this] val (firstArgumentXIndex, firstArgumentYIndex) = firstArgumentIndices
    private[this] val (secondArgumentXIndex, secondArgumentYIndex) = secondArgumentIndices

    override def execute(values: Array[Double]): Unit =
      values(resultIndex) =
        values(firstArgumentXIndex) * values(secondArgumentYIndex) -
        values(firstArgumentYIndex) * values(secondArgumentXIndex)

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) =
        values(firstArgumentXIndex) * values(secondArgumentYIndex) -
        values(firstArgumentYIndex) * values(secondArgumentXIndex)
  }

  private[ExpressionCompiler] class SquaredNorm2d(argumentIndices: (Int, Int), resultIndex: Int)
    extends ArrayOperation {

    private[this] val (argumentXIndex, argumentYIndex) = argumentIndices

    override def execute(values: Array[Double]): Unit = {
      val argumentX = values(argumentXIndex)
      val argumentY = values(argumentYIndex)
      values(resultIndex) = argumentX * argumentX + argumentY * argumentY
    }

    override def execute(values: Array[Interval]): Unit =
      values(resultIndex) = values(argumentXIndex).squared + values(argumentYIndex).squared
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
