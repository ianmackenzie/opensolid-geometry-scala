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

import scala.annotation.tailrec
import scala.math

trait Curve1d extends Bounded[Interval] {
  import Curve1d._

  def parameterized: ParametricCurve1d

  def parameterizedBy(
    expression: ScalarExpression[CurveParameter],
    domain: Interval
  ): ParametricCurve1d =
    new ParametricCurve1d(expression, domain) {
      override def bounds: Interval =
        Curve1d.this.bounds
    }

  def zeros(tolerance: Double, maxOrder: Int = 2): Zeros = {
    val parameterizedCurve = this.parameterized
    val expression = parameterizedCurve.expression
    val domain = parameterizedCurve.domain
    val derivatives =
      for {
        derivative <- Array.iterate(expression, maxOrder + 2)(_.derivative(CurveParameter))
      } yield {
        ScalarExpression.compileCurve(derivative)
      }

    // Calculate tolerances for each derivative order: tolerance(n) = n! * tolerance / width^n
    val tolerances = Array.ofDim[Double](maxOrder + 2)
    tolerances(0) = tolerance
    for (n <- 1 to maxOrder + 1) {
      tolerances(n) = n * tolerances(n - 1) / domain.width
    }

    // Check left end
    val leftXValue = domain.lowerBound
    val leftIsZero = derivatives(0).evaluate(leftXValue).isZero(tolerance)
    val leftRoot = if (leftIsZero) rootAt(leftXValue, derivatives, tolerances, 0) else None
    if (leftIsZero && leftRoot.isEmpty) {
      // Curve is identically zero
      EntireCurve
    } else {
      // Check right end
      val rightXValue = domain.upperBound
      val rightIsZero = derivatives(0).evaluate(rightXValue).isZero(tolerance)
      val rightRoot = if (rightIsZero) rootAt(rightXValue, derivatives, tolerances, 0) else None
      if (rightIsZero && rightRoot.isEmpty) {
        // Curve is identically zero
        EntireCurve
      } else {
        // Search for roots
        val (leftRoots, leftBound) = leftRoot match {
          case None =>
            (List.empty[Root], leftXValue)
          case Some(RootWithRadius(root, radius)) =>
            (List(root), leftXValue + radius)
        }
        val (rightRoots, rightBound) = rightRoot match {
          case None =>
            (List.empty[Root], rightXValue)
          case Some(RootWithRadius(root, radius)) =>
            (List(root), rightXValue - radius)
        }
        val fullInterval = Interval(leftBound, rightBound)
        val midRoots = allRootsWithin(fullInterval, derivatives, tolerances, maxOrder).map(_.root)
        Roots(leftRoots ++ midRoots ++ rightRoots)
      }
    }
  }
}

object Curve1d {
  sealed trait Zeros

  object EntireCurve extends Zeros

  case class Roots(roots: List[Root]) extends Zeros

  case class Root(x: Double, order: Int, sign: Int)

  private[Curve1d] case class RootWithRadius(root: Root, radiusOfInfluence: Double)

  @tailrec
  private[Curve1d] def rootAt(
    xValue: Double,
    derivatives: Array[ScalarExpression.CompiledCurve],
    tolerances: Array[Double],
    minOrder: Int
  ): Option[RootWithRadius] = {
    val derivativeOrder = minOrder + 1
    if (derivativeOrder >= derivatives.size) {
      None
    } else {
      val derivativeValue = derivatives(derivativeOrder).evaluate(xValue)
      if (derivativeValue.isNotZero(tolerances(derivativeOrder))) {
        val radius = radiusOfInfluence(derivativeOrder, derivativeValue, tolerances(0))
        Some(RootWithRadius(Root(xValue, minOrder, derivativeValue.signum), radius))
      } else {
        rootAt(xValue, derivatives, tolerances, derivativeOrder)
      }
    }
  }

  private[this] def radiusOfInfluence(
    order: Int,
    derivativeValue: Double,
    tolerance: Double
  ): Double = {
    val ratio = (tolerance / derivativeValue).abs
    order match {
      case 1 =>
        ratio
      case 2 =>
        math.sqrt(2 * ratio)
      case 3 =>
        math.cbrt(6 * ratio)
      case 4 =>
        math.sqrt(math.sqrt(24 * ratio))
      case _ =>
        throw new MatchError(s"Radius of influence not implemented for order $order")
    }
  }

  private[Curve1d] def allRootsWithin(
    xInterval: Interval,
    derivatives: Array[ScalarExpression.CompiledCurve],
    tolerances: Array[Double],
    order: Int
  ): List[RootWithRadius] = {
    val roots = rootsWithin(xInterval, derivatives, tolerances, order)
    order match {
      case 0 =>
        roots
      case _ => {
        val (tail, upperBound) =
          roots.foldRight((List.empty[RootWithRadius], xInterval.upperBound))(
            (left, accumulator) => accumulator match {
              case (results, upperBound) => {
                val interval = Interval(left.root.x + left.radiusOfInfluence, upperBound)
                val lowerOrder = allRootsWithin(interval, derivatives, tolerances, order - 1)
                (left +: lowerOrder ::: results, left.root.x - left.radiusOfInfluence)
              }
            }
          )
        val headInterval = Interval(xInterval.lowerBound, upperBound)
        val head = allRootsWithin(headInterval, derivatives, tolerances, order - 1)
        head ++ tail
      }
    }
  }

  private[Curve1d] def rootsWithin(
    xInterval: Interval,
    derivatives: Array[ScalarExpression.CompiledCurve],
    tolerances: Array[Double],
    order: Int
  ): List[RootWithRadius] = {
    val derivativeBounds = evaluateWithin(xInterval, derivatives)
    val lowerOrderNonZero =
      (0 to order - 1).exists(index => !derivativeBounds(index).contains(0.0, tolerances(index)))
    if (lowerOrderNonZero || !derivativeBounds(order).contains(0.0)) {
      // No roots of the given order exist since a derivative of that order or lower is non-zero
      List.empty[RootWithRadius]
    } else {
      // Attempt to bisect
      val xMin = xInterval.lowerBound
      val xMid = xInterval.midpoint
      val xMax = xInterval.upperBound
      if (xMin < xMid && xMid < xMax) {
        // X interval is large enough to bisect - recurse into each half
        val leftInterval = Interval(xMin, xMid)
        val rightInterval = Interval(xMid, xMax)
        val leftRoots = rootsWithin(leftInterval, derivatives, tolerances, order)
        val rightRoots = rootsWithin(rightInterval, derivatives, tolerances, order)
        mergeRoots(leftRoots, rightRoots)
      } else {
        // Cannot bisect any further - check if a root has been found
        val nextDerivativeValue = derivatives(order + 1).evaluate(xMid)
        val radius = radiusOfInfluence(order + 1, nextDerivativeValue, tolerances(0))
        List(RootWithRadius(Root(xMid, order, nextDerivativeValue.signum), radius))
      }
    }
  }

  private[this] def evaluateWithin(
    domain: Interval,
    derivatives: Array[ScalarExpression.CompiledCurve]
  ): Array[Interval] = {
    val result = Array.ofDim[Interval](derivatives.size)
    val maxOrder = derivatives.size - 1
    result(maxOrder) = derivatives(maxOrder).evaluateBounds(domain)
    for (order <- maxOrder - 1 to 0 by -1) {
      val derivative = derivatives(order)
      result(order) = if (result(order + 1).contains(0.0)) {
        derivative.evaluateBounds(domain)
      } else {
        // Next derivative is never zero in this interval, i.e. this derivative is monotonic - can
        // evaluate bounds by looking at endpoints only
        val leftValue = derivative.evaluate(domain.lowerBound)
        val rightValue = derivative.evaluate(domain.upperBound)
        leftValue.hull(rightValue)
      }
    }
    result
  }

  private[this] def mergeRoots(
    leftRoots: List[RootWithRadius],
    rightRoots: List[RootWithRadius]
  ): List[RootWithRadius] = leftRoots match {
    case Nil =>
      rightRoots
    case _ => {
      val leftReversed = leftRoots.reverse
      val leftLast :: leftInitReversed = leftReversed
      rightRoots match {
        case Nil =>
          leftRoots
        case rightHead :: rightTail => {
          val leftUpper = leftLast.root.x + leftLast.radiusOfInfluence
          val rightLower = rightHead.root.x - rightHead.radiusOfInfluence
          if (leftUpper >= rightLower) {
            // Last left root is equal to right head root - discard last left root
            leftInitReversed reverse_::: rightRoots
          } else {
            // No duplication - keep all
            leftReversed reverse_::: rightRoots
          }
        }
      }
    }
  }
}
