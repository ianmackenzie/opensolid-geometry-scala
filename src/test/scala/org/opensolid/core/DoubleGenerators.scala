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

import org.scalacheck._

object DoubleGenerators {
  val randomDouble: Gen[Double] =
    for {
      x <- Gen.chooseNum(-1.0, 1.0)
      y <- Gen.chooseNum(0.0, math.log(1e8))
    } yield x * math.exp(y)

  implicit val arbitraryDouble: Arbitrary[Double] = Arbitrary(randomDouble)

  def sortedValues(count: Integer): Gen[List[Double]] =
    Gen.listOfN[Double](count, randomDouble).map(list => list.sorted).suchThat(_.length == count)
}
