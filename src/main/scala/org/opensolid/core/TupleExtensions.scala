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

object TupleExtensions {
  implicit class Tuple2Extensions[T](val tuple: (T, T)) extends AnyVal {
    def map[U](function: (T) => U): (U, U) = tuple match {
      case (first, second) => (function(first), function(second))
    }

    def withFilter(predicate: (T) => Boolean): Traversable[T] =
      new Traversable[T] {
        override def foreach[U](function: (T) => U): Unit = tuple match {
          case (first, second) => {
            if (predicate(first)) function(first)
            if (predicate(second)) function(second)
          }
        }
      }

    def foreach[U](function: (T) => U): Unit = tuple match {
      case (first, second) => {
        function(first)
        function(second)
      }
    }
  }

  implicit class Tuple3Extensions[T](val tuple: (T, T, T)) extends AnyVal {
    def map[U](function: (T) => U): (U, U, U) = tuple match {
      case (first, second, third) => (function(first), function(second), function(third))
    }

    def withFilter(predicate: (T) => Boolean): Traversable[T] =
      new Traversable[T] {
        override def foreach[U](function: (T) => U): Unit = tuple match {
          case (first, second, third) => {
            if (predicate(first)) function(first)
            if (predicate(second)) function(second)
            if (predicate(third)) function(third)
          }
        }
      }

    def foreach(function: (T) => Unit): Unit = tuple match {
      case (first, second, third) => {
        function(first)
        function(second)
        function(third)
      }
    }
  }
}
