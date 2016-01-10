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
import net.bytebuddy.implementation.bytecode.ByteCodeAppender
import net.bytebuddy.implementation.Implementation
import net.bytebuddy.matcher.ElementMatchers

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

import org.opensolid.core._

object Assembler {
  private[this] var classIndex = 0

  private[this] def newClassName = {
    classIndex = classIndex + 1
    "GeneratedFunction$classIndex"
  }

  private[this] def applyImplementation(
    className: String,
    buildResult: Builder.Result,
    resultValues: Array[Value]
  ): FunctionImplementation = {
    val classDescriptor = s"Lorg/opensolid/core/codegen/$className;"
    new FunctionImplementation(classDescriptor) {
      def apply(methodVisitor: MethodVisitor): ByteCodeAppender.Size = {
        for (TemporaryAssignment(result, expression) <- buildResult.temporaryAssignments) {
          expression match {
            case Negation(value) => pushUnaryOperation(Opcodes.DNEG)
            case Sum(first, second) => pushBinaryOperation(Opcodes.DADD)
            case Difference(first, second) => pushBinaryOperation(Opcodes.DSUB)
            case Product(first, second) => pushBinaryOperation(Opcodes.DMUL)
            case Quotient(first, second) => pushBinaryOperation(Opcodes.DDIV)
            case Square(value) =>
            case SquareRoot(value) =>
            case Sine(value) =>
            case Cosine(value) =>
          }
        }
        new ByteCodeAppender.Size(-1, -1) // TODO
      }
    }
  }

  private[this] def newSubclassOf[T](
    baseClass: java.lang.Class[T],
    className: String,
    //constructorImplementation: FunctionImplementation,
    applyImplementation: FunctionImplementation
  ): Class[_ <: T] = {
    new ByteBuddy()
      .subclass(baseClass)
      .name(s"org.opensolid.core.codegen.$className")
      .method(ElementMatchers.named("apply"))
      .intercept(applyImplementation)
      .make()
      .load(baseClass.getClassLoader(), ClassLoadingStrategy.Default.WRAPPER)
      .getLoaded()
  }

  def assemble(expression: CurveExpression1d): Class[_ <: AbstractFunction1[Double, Double]] = {
    // TODO cache results
    val builder = new Builder
    val resultValue = expression.build(builder)
    val className = newClassName
    val applyImpl = applyImplementation(className, builder.result, Array(resultValue))
    val baseClass = classOf[AbstractFunction1[Double, Double]]
    newSubclassOf(baseClass, className, applyImpl)
  }
}
