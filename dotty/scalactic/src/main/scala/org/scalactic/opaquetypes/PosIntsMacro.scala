package org.scalactic.opaquetypes

import scala.quoted.*
import org.scalactic.anyvals.CompileTimeAssertions.*

object PosIntsMacro {

  def isValid(i: Int): Boolean = i >= 0

  def posZIntApply(value: Expr[Int])(using Quotes): Expr[PosInts.PosZInt] = {
    val notValidMsg =
      "PosZInt.apply can only be invoked on non-negative Int literals, like PosZInt(8)."
    val notLiteralMsg =
      "PosZInt.apply can only be invoked on Int literals, like PosZInt(8). Please use PosZInt.from instead."
    ensureValidIntLiteral(value, notValidMsg, notLiteralMsg)(isValid)
    '{ PosInts.PosZInt.ensuringValid($value) }
  }

  def posZIntConversion(value: Expr[Int])(using Quotes): Expr[PosInts.PosZInt] = {
    val notValidMsg =
      "PosZInt conversion can only be invoked on non-negative Int literals, like PosZInt(8)."
    val notLiteralMsg =
      "PosZInt conversion requires an Int literal. Please use PosZInt.ensuringValid for runtime Int values."
    ensureValidIntLiteral(value, notValidMsg, notLiteralMsg)(isValid)
    '{ PosInts.PosZInt.ensuringValid($value) }
  }
}