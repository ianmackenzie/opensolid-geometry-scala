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

class SpatialSet[T <: Bounded[B] with GeometricallyComparable[T], B : Bounds](items: Traversable[T])
  extends IndexedSeq[T] {

  private[this] val leaves = items.map(new SpatialTree.Leaf[T, B](_)).toArray

  private[this] val traits = implicitly[Bounds[B]]

  val root = SpatialTree.build[T, B](leaves, 0, leaves.size, traits, 0)

  override def toString: String = s"SpatialSet($root)"

  override def length: Int = leaves.length

  override def apply(index: Int): T = leaves(index).item

  def withBoundsFilter(predicate: (B) => Boolean): Iterable[T] =
    new SpatialSet.Filtered[T, B](root, predicate)

  def contains(argument: T, tolerance: Double): Boolean = {
    val argumentBounds = argument.bounds
    val boundsFilter = (itemBounds: B) => traits.overlaps(itemBounds, argumentBounds, tolerance)
    withBoundsFilter(boundsFilter).exists(item => item.equals(argument, tolerance))
  }
}

object SpatialSet {
  final class Filtered[T <: Bounded[B] with GeometricallyComparable[T], B : Bounds](
    root: SpatialTree[T, B],
    predicate: (B) => Boolean
  ) extends Iterable[T] {

    override def foreach[U](function: (T) => U): Unit = foreach(root, function)

    private[this] def foreach[U](tree: SpatialTree[T, B], function: (T) => U): Unit = tree match {
      case leaf: SpatialTree.Leaf[T, B] => if (predicate(leaf.bounds)) function(leaf.item)
      case node: SpatialTree.Node[T, B] =>
        if (predicate(node.bounds)) {
          foreach(node.leftChild, function)
          foreach(node.rightChild, function)
        }
      case empty: SpatialTree.Empty[T, B] => ()
    }

    override def iterator: Iterator[T] = iterator(root)

    private[this] def iterator(tree: SpatialTree[T, B]): Iterator[T] = tree match {
      case leaf: SpatialTree.Leaf[T, B] =>
        if (predicate(leaf.bounds)) Iterator.single(leaf.item) else Iterator.empty
      case node: SpatialTree.Node[T, B] =>
        if (predicate(node.bounds)) {
          iterator(node.leftChild) ++ iterator(node.rightChild)
        } else {
          Iterator.empty
        }
      case empty: SpatialTree.Empty[T, B] => Iterator.empty
    }
  }
}
