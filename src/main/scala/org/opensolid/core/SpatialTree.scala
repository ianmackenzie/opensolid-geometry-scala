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

sealed abstract class SpatialTree[T <: Bounded[B] with GeometricallyComparable[T], B <: Bounds[B]]
  extends Iterable[T] {

  def bounds: B

  def withBoundsFilter(predicate: (B) => Boolean): Iterable[T] =
    new SpatialTree.Filtered[T, B](this, predicate)

  def contains(argument: T, tolerance: Double): Boolean = {
    val argumentBounds = argument.bounds
    withBoundsFilter(_.overlaps(argumentBounds, tolerance)).exists(_.equals(argument, tolerance))
  }
}

object SpatialTree {
  def build[T <: Bounded1d with GeometricallyComparable[T] : OneDimensional](
    items: Seq[T]
  ): SpatialTree[T, Interval] = {
    val leaves = items.map(item => new Leaf[T, Interval](item, item.bounds)).toArray
    build(leaves, Interval.Empty, 0, leaves.size, 0, 1)
  }

  def build[T <: Bounded2d with GeometricallyComparable[T] : TwoDimensional](
    items: Seq[T]
  ): SpatialTree[T, Bounds2d] = {
    val leaves = items.map(item => new Leaf[T, Bounds2d](item, item.bounds)).toArray
    build(leaves, Bounds2d.Empty, 0, leaves.size, 0, 2)
  }

  def build[T <: Bounded3d with GeometricallyComparable[T] : ThreeDimensional](
    items: Seq[T]
  ): SpatialTree[T, Bounds3d] = {
    val leaves = items.map(item => new Leaf[T, Bounds3d](item, item.bounds)).toArray
    build(leaves, Bounds3d.Empty, 0, leaves.size, 0, 3)
  }

  final case class Node[T <: Bounded[B] with GeometricallyComparable[T], B <: Bounds[B]](
    leftChild: SpatialTree[T, B],
    rightChild: SpatialTree[T, B]
  ) extends SpatialTree[T, B] {

    override def toString: String =
      s"Node($leftChild, $rightChild)"

    override val size: Int = leftChild.size + rightChild.size

    override val bounds = leftChild.bounds.hull(rightChild.bounds)

    override def foreach[U](function: (T) => U): Unit = {
      leftChild.foreach(function)
      rightChild.foreach(function)
    }

    override def iterator: Iterator[T] =
      leftChild.iterator ++ rightChild.iterator
  }

  final case class Leaf[T <: Bounded[B] with GeometricallyComparable[T], B <: Bounds[B]](
    val item: T,
    override val bounds: B
  ) extends SpatialTree[T, B] {

    override def toString: String =
      s"Leaf($item)"

    override def size: Int =
      1

    override def foreach[U](function: (T) => U): Unit =
      function(item)

    override def iterator: Iterator[T] =
      Iterator.single(item)
  }

  final case class Empty[T <: Bounded[B] with GeometricallyComparable[T], B <: Bounds[B]](
    override val bounds: B
  ) extends SpatialTree[T, B] {

    override def toString: String =
      "Empty"

    override def size: Int =
      0

    override def foreach[U](function: (T) => U): Unit =
      ()

    override def iterator: Iterator[T] =
      Iterator.empty
  }

  private[SpatialTree] final class Filtered[
    T <: Bounded[B] with GeometricallyComparable[T],
    B <: Bounds[B]
  ](root: SpatialTree[T, B], predicate: (B) => Boolean) extends Iterable[T] {

    override def foreach[U](function: (T) => U): Unit =
      foreach(root, function)

    private[this] def foreach[U](tree: SpatialTree[T, B], function: (T) => U): Unit = tree match {
      case leaf: Leaf[T, B] =>
        if (predicate(leaf.bounds)) function(leaf.item)
      case node: Node[T, B] =>
        if (predicate(node.bounds)) {
          foreach(node.leftChild, function)
          foreach(node.rightChild, function)
        }
      case empty: Empty[T, B] =>
        ()
    }

    override def iterator: Iterator[T] =
      iterator(root)

    private[this] def iterator(tree: SpatialTree[T, B]): Iterator[T] = tree match {
      case leaf: Leaf[T, B] =>
        if (predicate(leaf.bounds)) Iterator.single(leaf.item) else Iterator.empty
      case node: Node[T, B] =>
        if (predicate(node.bounds)) {
          iterator(node.leftChild) ++ iterator(node.rightChild)
        } else {
          Iterator.empty
        }
      case empty: Empty[T, B] =>
        Iterator.empty
    }
  }

  private[this] def build[T <: Bounded[B] with GeometricallyComparable[T], B <: Bounds[B]](
    leaves: Array[Leaf[T, B]],
    emptyBounds: B,
    beginIndex: Int,
    endIndex: Int,
    dimensionIndex: Int,
    numDimensions: Int
  ): SpatialTree[T, B] = (endIndex - beginIndex) match {
    case 0 => new Empty[T, B](emptyBounds)
    case 1 => leaves(beginIndex)
    case 2 => {
      val firstLeaf = leaves(beginIndex)
      val secondLeaf = leaves(beginIndex + 1)
      if (firstLeaf.bounds.hasLesserMedianThan(secondLeaf.bounds, dimensionIndex)) {
        new Node(firstLeaf, secondLeaf)
      } else {
        new Node(secondLeaf, firstLeaf)
      }
    }
    case _ => {
      val midIndex = beginIndex + (endIndex - beginIndex) / 2
      bisect(leaves, beginIndex, midIndex, endIndex, dimensionIndex)
      val nextDimensionIndex = (dimensionIndex + 1) % numDimensions
      new Node(
        build(leaves, emptyBounds, beginIndex, midIndex, nextDimensionIndex, numDimensions),
        build(leaves, emptyBounds, midIndex, endIndex, nextDimensionIndex, numDimensions)
      )
    }
  }

  private[this] def bisect[T <: Bounded[B] with GeometricallyComparable[T], B <: Bounds[B]](
    leaves: Array[Leaf[T, B]],
    beginIndex: Int,
    midIndex: Int,
    endIndex: Int,
    dimensionIndex: Int
  ): Unit = {
    var (lesserEndIndex, equalEndIndex) =
      partition(leaves, beginIndex, endIndex, dimensionIndex)
    if (midIndex < lesserEndIndex) {
      bisect(leaves, beginIndex, midIndex, lesserEndIndex, dimensionIndex)
    } else if (midIndex > equalEndIndex) {
      bisect(leaves, equalEndIndex, midIndex, endIndex, dimensionIndex)
    } else {
      () // Leaves are bisected, return
    }
  }

  private[this] def partition[T <: Bounded[B] with GeometricallyComparable[T], B <: Bounds[B]](
    leaves: Array[Leaf[T, B]],
    beginIndex: Int,
    endIndex: Int,
    dimensionIndex: Int
  ): (Int, Int) = (endIndex - beginIndex) match {
    case 0 => (endIndex, endIndex)
    case 1 => (beginIndex, endIndex)
    case _ => {
      val pivotIndex = medianIndex(leaves, beginIndex, endIndex, dimensionIndex)
      val pivotBounds = leaves(pivotIndex).bounds
      var lesserEndIndex = beginIndex
      for { index <- beginIndex.until(endIndex) } {
        if (leaves(index).bounds.hasLesserMedianThan(pivotBounds, dimensionIndex)) {
          swap(leaves, index, lesserEndIndex)
          lesserEndIndex += 1
        }
      }
      var equalEndIndex = lesserEndIndex
      for { index <- equalEndIndex.until(endIndex) } {
        if (leaves(index).bounds.hasEqualMedianTo(pivotBounds, dimensionIndex)) {
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

  private[this] def medianIndex[T <: Bounded[B] with GeometricallyComparable[T], B <: Bounds[B]](
    leaves: Array[Leaf[T, B]],
    beginIndex: Int,
    endIndex: Int,
    dimensionIndex: Int
  ): Int = {
    val midIndex = beginIndex + (endIndex - beginIndex) / 2
    val lastIndex = endIndex - 1
    val firstLeaf = leaves(beginIndex)
    val midLeaf = leaves(midIndex)
    val lastLeaf = leaves(lastIndex)
    if (firstLeaf.bounds.hasLesserMedianThan(midLeaf.bounds, dimensionIndex)) {
      if (midLeaf.bounds.hasLesserMedianThan(lastLeaf.bounds, dimensionIndex)) {
        midIndex
      } else if (firstLeaf.bounds.hasLesserMedianThan(lastLeaf.bounds, dimensionIndex)) {
        lastIndex
      } else {
        beginIndex
      }
    } else {
      if (firstLeaf.bounds.hasLesserMedianThan(lastLeaf.bounds, dimensionIndex)) {
        beginIndex
      } else if (midLeaf.bounds.hasLesserMedianThan(lastLeaf.bounds, dimensionIndex)) {
        lastIndex
      } else {
        midIndex
      }
    }
  }
}
