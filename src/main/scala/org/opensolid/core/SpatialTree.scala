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

sealed abstract class SpatialTree[T <: Bounded[B], B : Bounds] extends Iterable[T] {
  import SpatialTree._

  def bounds: B
}

object SpatialTree {
  final case class Node[T <: Bounded[B], B: Bounds](
    leftChild: SpatialTree[T, B],
    rightChild: SpatialTree[T, B]
  ) extends SpatialTree[T, B] {

    override def toString: String = s"Node($leftChild, $rightChild)"

    override val size: Int = leftChild.size + rightChild.size

    override val bounds = implicitly[Bounds[B]].hull(leftChild.bounds, rightChild.bounds)

    override def foreach[U](function: (T) => U): Unit = {
      leftChild.foreach(function)
      rightChild.foreach(function)
    }

    override def iterator: Iterator[T] = leftChild.iterator ++ rightChild.iterator
  }

  final case class Leaf[T <: Bounded[B], B : Bounds](val item: T) extends SpatialTree[T, B] {
    override def toString: String = s"Leaf($item)"

    override def size: Int = 1

    override val bounds = item.bounds

    override def foreach[U](function: (T) => U): Unit = function(item)

    override def iterator: Iterator[T] = Iterator.single(item)
  }

  final case class Empty[T <: Bounded[B], B : Bounds]() extends SpatialTree[T, B] {
    override def toString: String = "Empty"

    override def size: Int = 0

    override def bounds: B = implicitly[Bounds[B]].Empty

    override def foreach[U](function: (T) => U): Unit = ()

    override def iterator: Iterator[T] = Iterator.empty
  }

  private[core] def build[T <: Bounded[B], B: Bounds](
    leaves: Array[Leaf[T, B]],
    beginIndex: Int,
    endIndex: Int,
    traits: Bounds[B],
    dimensionIndex: Int
  ): SpatialTree[T, B] = (endIndex - beginIndex) match {
    case 0 => new Empty[T, B]
    case 1 => leaves(beginIndex)
    case 2 => {
      val firstLeaf = leaves(beginIndex)
      val secondLeaf = leaves(beginIndex + 1)
      if (traits.hasLesserMedian(firstLeaf.bounds, secondLeaf.bounds, dimensionIndex)) {
        new Node(firstLeaf, secondLeaf)
      } else {
        new Node(secondLeaf, firstLeaf)
      }
    }
    case _ => {
      val midIndex = beginIndex + (endIndex - beginIndex) / 2
      bisect(leaves, beginIndex, midIndex, endIndex, traits, dimensionIndex)
      val nextDimensionIndex = (dimensionIndex + 1) % traits.NumDimensions
      new Node(
        build(leaves, beginIndex, midIndex, traits, nextDimensionIndex),
        build(leaves, midIndex, endIndex, traits, nextDimensionIndex)
      )
    }
  }

  private[this] def bisect[T <: Bounded[B], B: Bounds](
    leaves: Array[Leaf[T, B]],
    beginIndex: Int,
    midIndex: Int,
    endIndex: Int,
    traits: Bounds[B],
    dimensionIndex: Int
  ): Unit = {
    var (lesserEndIndex, equalEndIndex) =
      partition(leaves, beginIndex, endIndex, traits, dimensionIndex)
    if (midIndex < lesserEndIndex) {
      bisect(leaves, beginIndex, midIndex, lesserEndIndex, traits, dimensionIndex)
    } else if (midIndex > equalEndIndex) {
      bisect(leaves, equalEndIndex, midIndex, endIndex, traits, dimensionIndex)
    } else {
      return
    }
  }

  private[this] def partition[T <: Bounded[B], B: Bounds](
    leaves: Array[Leaf[T, B]],
    beginIndex: Int,
    endIndex: Int,
    traits: Bounds[B],
    dimensionIndex: Int
  ): (Int, Int) = (endIndex - beginIndex) match {
    case 0 => (endIndex, endIndex)
    case 1 => (beginIndex, endIndex)
    case _ => {
      val pivotIndex = medianIndex(leaves, beginIndex, endIndex, traits, dimensionIndex)
      val pivotBounds = leaves(pivotIndex).bounds
      var lesserEndIndex = beginIndex
      for (index <- beginIndex.until(endIndex)) {
        if (traits.hasLesserMedian(leaves(index).bounds, pivotBounds, dimensionIndex)) {
          swap(leaves, index, lesserEndIndex)
          lesserEndIndex += 1
        }
      }
      var equalEndIndex = lesserEndIndex
      for (index <- equalEndIndex.until(endIndex)) {
        if (traits.hasEqualMedian(leaves(index).bounds, pivotBounds, dimensionIndex)) {
          swap(leaves, index, equalEndIndex)
          equalEndIndex += 1
        }
      }
      (lesserEndIndex, equalEndIndex)
    }
  }

  private[this] def swap[T](array: Array[T], firstIndex: Int, secondIndex: Int): Unit = {
    if (firstIndex != secondIndex) {
      val firstItem = array(firstIndex)
      val secondItem = array(secondIndex)
      array(firstIndex) = secondItem
      array(secondIndex) = firstItem
    }
  }

  private[this] def medianIndex[T <: Bounded[B], B : Bounds](
    leaves: Array[Leaf[T, B]],
    beginIndex: Int,
    endIndex: Int,
    traits: Bounds[B],
    dimensionIndex: Int
  ): Int = {
    val midIndex = beginIndex + (endIndex - beginIndex) / 2
    val lastIndex = endIndex - 1
    val firstLeaf = leaves(beginIndex)
    val midLeaf = leaves(midIndex)
    val lastLeaf = leaves(lastIndex)
    if (traits.hasLesserMedian(firstLeaf.bounds, midLeaf.bounds, dimensionIndex)) {
      if (traits.hasLesserMedian(midLeaf.bounds, lastLeaf.bounds, dimensionIndex)) {
        midIndex
      } else if (traits.hasLesserMedian(firstLeaf.bounds, lastLeaf.bounds, dimensionIndex)) {
        lastIndex
      } else {
        beginIndex
      }
    } else {
      if (traits.hasLesserMedian(firstLeaf.bounds, lastLeaf.bounds, dimensionIndex)) {
        beginIndex
      } else if (traits.hasLesserMedian(midLeaf.bounds, lastLeaf.bounds, dimensionIndex)) {
        lastIndex
      } else {
        midIndex
      }
    }
  }
}
