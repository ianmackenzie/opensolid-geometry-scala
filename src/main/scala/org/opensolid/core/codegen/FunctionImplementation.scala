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

import net.bytebuddy.description.method.MethodDescription
import net.bytebuddy.dynamic.scaffold.InstrumentedType
import net.bytebuddy.implementation.bytecode.ByteCodeAppender
import net.bytebuddy.implementation.Implementation
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes

import org.opensolid.core._

abstract class FunctionImplementation(classDescriptor: String)
  extends Implementation with ByteCodeAppender {

  override def prepare(instrumentedType: InstrumentedType): InstrumentedType = instrumentedType

  override def appender(target: Implementation.Target): ByteCodeAppender = this

  def pushValue(methodVisitor: MethodVisitor, value: Value): Unit = value match {
    case Constant(value) => methodVisitor.visitLdcInsn(value)
    case Parameter(index) => {
      methodVisitor.visitVarInsn(Opcodes.ALOAD, 1)
      methodVisitor.visitInsn(Opcodes.ICONST_1)
      methodVisitor.visitInsn(Opcodes.AALOAD)
    }
    case Field(index) =>
      methodVisitor.visitFieldInsn(Opcodes.GETFIELD, classDescriptor, s"f$index", "D")
    case Temporary(index) => methodVisitor.visitVarIsn(Opcodes.ALOAD, 2 + index)
  }

  def pushUaryOperation(methodVisitor: MethodVisitor, value: Value, opcode: Int): Unit = {
    pushValue(value)
    methodVisitor.visitInsn(opcode)
  }

  def pushBinaryOperation(
    methodVisitor: MethodVisitor,
    first: Value,
    second: Value,
    opcode: Int
  ): Unit = {
    pushValue(first)
    pushValue(second)
    methodVisitor.visitInsn(opcode)
  }

  override def apply(
    methodVisitor: MethodVisitor,
    context: Implementation.Context,
    target: MethodDescription
  ): ByteCodeAppender.Size = apply(methodVisitor)

  def apply(methodVisitor: MethodVisitor): ByteCodeAppender.Size
}
