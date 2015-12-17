//////////////////////////////////////////////////////////////////////////////
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

package org.opensolid.core.codegen

import scala.runtime.AbstractFunction1

import net.bytebuddy.ByteBuddy
import net.bytebuddy.dynamic.loading.ClassLoadingStrategy
import net.bytebuddy.implementation.FixedValue
import net.bytebuddy.matcher.ElementMatchers

import org.opensolid.core._

object Compiler {
  def compile(expression: CurveExpression1d): CurveFunction1d = {
    val classLoader = classOf[AbstractFunction1[Double, Double]].getClassLoader()
    val generated = new ByteBuddy()
      .subclass(classOf[AbstractFunction1[Double, Double]])
      .method(ElementMatchers.named("apply"))
      .intercept(FixedValue.value(0.0))
      .make()
      .load(classLoader, ClassLoadingStrategy.Default.WRAPPER)
      .getLoaded()
      .newInstance()
      .asInstanceOf[Function1[Double, Double]]
    CurveFunction1d(generated)
  }
}
