/*
 * Copyright 2001-2014 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalactic.anyvals

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._
import org.scalactic.Equality
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import OptionValues._

import scala.util.{Failure, Success, Try}

class NonZeroIntSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      case Success(double: Double) if double.isNaN =>  // This is because in scala.js x/0 results to NaN not ArithmetricException like in jvm, and we need to make sure Success(NaN) == Success(NaN) is true to pass the test.
        b match {
          case Success(bDouble: Double) if bDouble.isNaN => true
          case _ => false
        }
      // I needed this because with GenDrivenPropertyChecks, got:
      // [info] - should offer a '%' method that is consistent with Int *** FAILED ***
      // [info]   Success(NaN) did not equal Success(NaN) (NonZeroIntExperiment.scala:498)
      case Success(float: Float) if float.isNaN =>
        b match {
          case Success(bFloat: Float) if bFloat.isNaN => true
          case _ => false
        }
      case _: Success[_] => a == b
      case Failure(ex) => b match {
        case _: Success[_] => false
        case Failure(otherEx) => ex.getClass == otherEx.getClass && ex.getMessage == otherEx.getMessage
        case _ => false
      }
    }
  }

  describe("A NonZeroInt") {

    describe("should offer a from factory method that") {
      it("returns Some[NonZeroInt] if the passed Int is greater than 0") {
        NonZeroInt.from(50).value.value shouldBe 50
        NonZeroInt.from(100).value.value shouldBe 100
      }

      it("returns Some[NonZeroInt] if the passed Int is lesser than 0") {
        NonZeroInt.from(-1).value.value shouldBe -1
        NonZeroInt.from(-99).value.value shouldBe -99
      }

      it("returns None if the passed Int is 0") {
        NonZeroInt.from(0) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NonZeroInt if the passed Int is greater than 0") {
        NonZeroInt.ensuringValid(50).value shouldBe 50
        NonZeroInt.ensuringValid(100).value shouldBe 100
      }

      it("returns NonZeroInt if the passed Int is lesser than 0") {
        NonZeroInt.ensuringValid(-1).value shouldBe -1
        NonZeroInt.ensuringValid(-99).value shouldBe -99
      }

      it("throws AssertionError if the passed Int is 0") {
        an [AssertionError] should be thrownBy NonZeroInt.ensuringValid(0)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Int is not 0") {
        NonZeroInt.isValid(50) shouldBe true
        NonZeroInt.isValid(100) shouldBe true
        NonZeroInt.isValid(0) shouldBe false
        NonZeroInt.isValid(-0) shouldBe false
        NonZeroInt.isValid(-1) shouldBe true
        NonZeroInt.isValid(-99) shouldBe true
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NonZeroInt if the passed Int is greater than 0") {
        NonZeroInt.fromOrElse(50, NonZeroInt(42)).value shouldBe 50
        NonZeroInt.fromOrElse(100, NonZeroInt(42)).value shouldBe 100
      }
      it("returns a NonZeroInt if the passed Int is leser than 0") {
        NonZeroInt.fromOrElse(-1, NonZeroInt(42)).value shouldBe -1
        NonZeroInt.fromOrElse(-99, NonZeroInt(42)).value shouldBe -99
      }
      it("returns a given default if the passed Int is 0") {
        NonZeroInt.fromOrElse(0, NonZeroInt(42)).value shouldBe 42
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NonZeroInt.MaxValue shouldEqual NonZeroInt.from(Int.MaxValue).get
      NonZeroInt.MinValue shouldEqual NonZeroInt(Int.MinValue)
    }
    it("should have a pretty toString") {
      NonZeroInt.from(42).value.toString shouldBe "NonZeroInt(42)"
    }

    it("should return the same type from its unary_+ method") {
      +NonZeroInt(3) shouldEqual NonZeroInt(3)
    }

    it("should be automatically widened to compatible AnyVal targets") {
      (NonZeroInt(3): Int) shouldEqual 3
      (NonZeroInt(3): Long) shouldEqual 3L
      (NonZeroInt(3): Float) shouldEqual 3.0F
      (NonZeroInt(3): Double) shouldEqual 3.0

      /*(NonZeroInt(3): NonZeroInt) shouldEqual NonZeroInt(3)
      (NonZeroInt(3): PosLong) shouldEqual PosLong(3L)
      (NonZeroInt(3): PosFloat) shouldEqual PosFloat(3.0F)
      (NonZeroInt(3): PosDouble) shouldEqual PosDouble(3.0)

      (NonZeroInt(3): PosZInt) shouldEqual PosZInt(3)
      (NonZeroInt(3): PosZLong) shouldEqual PosZLong(3L)
      (NonZeroInt(3): PosZFloat) shouldEqual PosZFloat(3.0F)
      (NonZeroInt(3): PosZDouble) shouldEqual PosZDouble(3.0)*/
    }

    it("should be sortable") {
      val xs = List(NonZeroInt(2), NonZeroInt(4), NonZeroInt(1), NonZeroInt(3))
      xs.sorted shouldEqual List(NonZeroInt(1), NonZeroInt(2), NonZeroInt(3), NonZeroInt(4))
    }

    describe("when a compatible AnyVal is passed to a + method invoked on it") {
      it("should give the same AnyVal type back at compile time, and correct value at runtime") {
        // When adding a "primitive"
        val opInt = NonZeroInt(3) + 3
        opInt shouldEqual 6

        val opLong = NonZeroInt(3) + 3L
        opLong shouldEqual 6L

        val opFloat = NonZeroInt(3) + 3.0F
        opFloat shouldEqual 6.0F

        val opDouble = NonZeroInt(3) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Pos*
        val opPosInt = NonZeroInt(3) + PosInt(3)
        opPosInt shouldEqual 6

        val opPosLong = NonZeroInt(3) + PosLong(3L)
        opPosLong shouldEqual 6L

        val opPosFloat = NonZeroInt(3) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0F

        val opPosDouble = NonZeroInt(3) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *PosZ
        val opPosZ = NonZeroInt(3) + PosZInt(3)
        opPosZ shouldEqual 6

        val opPosZLong = NonZeroInt(3) + PosZLong(3L)
        opPosZLong shouldEqual 6L

        val opPosZFloat = NonZeroInt(3) + PosZFloat(3.0F)
        opPosZFloat shouldEqual 6.0F

        val opPosZDouble = NonZeroInt(3) + PosZDouble(3.0)
        opPosZDouble shouldEqual 6.0

        // When adding a NonZero*
        val opNonZeroInt = NonZeroInt(3) + NonZeroInt(3)
        opNonZeroInt shouldEqual 6

        /*val opNonZeroLong = NonZeroInt(3) + NonZeroLong(3L)
        opNonZeroLong shouldEqual 6L

        val opNonZeroFloat = NonZeroInt(3) + NonZeroFloat(3.0F)
        opNonZeroFloat shouldEqual 6.0F

        val opNonZeroDouble = NonZeroInt(3) + NonZeroDouble(3.0)
        opNonZeroDouble shouldEqual 6.0*/
      }
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "NonZeroInt(8)" should compile
        NonZeroInt(8).value shouldEqual 8
      }

      it("should not compile when 0 is passed in") {
        "NonZeroInt(0)" shouldNot compile
      }

      it("should compile when -8 is passed in") {
        "NonZeroInt(-8)" should compile
        NonZeroInt(-8).value shouldEqual -8
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "NonZeroInt(x)" shouldNot compile
      }
    }

    describe("when specified as a plain-old Int") {

      def takesNonZeroInt(non0: NonZeroInt): Int = non0.value

      it("should compile when 8 is passed in") {
        "takesNonZeroInt(8)" should compile
        takesNonZeroInt(8) shouldEqual 8
      }

      it("should not compile when 0 is passed in") {
        "takesNonZeroInt(0)" shouldNot compile
      }

      it("should compile when -8 is passed in") {
        "takesNonZeroInt(-8)" should compile
        takesNonZeroInt(-8) shouldEqual -8
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNonZeroInt(x)" shouldNot compile
      }
    }

    it("should offer a unary ~ method that is consistent with Int") {
      forAll { (nzint: NonZeroInt) =>
        (~nzint) shouldEqual (~(nzint.toInt))
      }
    }

    it("should offer a unary + method that is consistent with Int") {
      forAll { (nzint: NonZeroInt) =>
        (+nzint).toInt shouldEqual (+(nzint.toInt))
      }
    }

    it("should offer a unary - method that is consistent with Int") {
      forAll { (nzint: NonZeroInt) =>
        (-nzint) shouldEqual (-(nzint.toInt))
      }
    }

    it("should offer << methods that are consistent with Int") {
      forAll { (nzint: NonZeroInt, shift: Int) =>
        nzint << shift shouldEqual nzint.toInt << shift
      }
      forAll { (nzint: NonZeroInt, shift: Long) =>
        nzint << shift shouldEqual nzint.toInt << shift
      }
    }

    it("should offer >>> methods that are consistent with Int") {
      forAll { (nzint: NonZeroInt, shift: Int) =>
        nzint >>> shift shouldEqual nzint.toInt >>> shift
      }
      forAll { (nzint: NonZeroInt, shift: Long) =>
        nzint >>> shift shouldEqual nzint.toInt >>> shift
      }
    }

    it("should offer >> methods that are consistent with Int") {
      forAll { (nzint: NonZeroInt, shift: Int) =>
        nzint >> shift shouldEqual nzint.toInt >> shift
      }
      forAll { (nzint: NonZeroInt, shift: Long) =>
        nzint >> shift shouldEqual nzint.toInt >> shift
      }
    }

    it("should offer '<' comparison that is consistent with Int") {
      forAll { (nzint: NonZeroInt, byte: Byte) =>
        (nzint < byte) shouldEqual (nzint.toInt < byte)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        (nzint < short) shouldEqual (nzint.toInt < short)
      }
      forAll { (nzint: NonZeroInt, char: Char) =>
        (nzint < char) shouldEqual (nzint.toInt < char)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        (nzint < int) shouldEqual (nzint.toInt < int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        (nzint < long) shouldEqual (nzint.toInt < long)
      }
      forAll { (nzint: NonZeroInt, float: Float) =>
        (nzint < float) shouldEqual (nzint.toInt < float)
      }
      forAll { (nzint: NonZeroInt, double: Double) =>
        (nzint < double) shouldEqual (nzint.toInt < double)
      }
    }

    it("should offer '<=' comparison that is consistent with Int") {
      forAll { (nzint: NonZeroInt, byte: Byte) =>
        (nzint <= byte) shouldEqual (nzint.toInt <= byte)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        (nzint <= short) shouldEqual (nzint.toInt <= short)
      }
      forAll { (nzint: NonZeroInt, char: Char) =>
        (nzint <= char) shouldEqual (nzint.toInt <= char)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        (nzint <= int) shouldEqual (nzint.toInt <= int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        (nzint <= long) shouldEqual (nzint.toInt <= long)
      }
      forAll { (nzint: NonZeroInt, float: Float) =>
        (nzint <= float) shouldEqual (nzint.toInt <= float)
      }
      forAll { (nzint: NonZeroInt, double: Double) =>
        (nzint <= double) shouldEqual (nzint.toInt <= double)
      }
    }

    it("should offer '>' comparison that is consistent with Int") {
      forAll { (nzint: NonZeroInt, byte: Byte) =>
        (nzint > byte) shouldEqual (nzint.toInt > byte)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        (nzint > short) shouldEqual (nzint.toInt > short)
      }
      forAll { (nzint: NonZeroInt, char: Char) =>
        (nzint > char) shouldEqual (nzint.toInt > char)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        (nzint > int) shouldEqual (nzint.toInt > int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        (nzint > long) shouldEqual (nzint.toInt > long)
      }
      forAll { (nzint: NonZeroInt, float: Float) =>
        (nzint > float) shouldEqual (nzint.toInt > float)
      }
      forAll { (nzint: NonZeroInt, double: Double) =>
        (nzint > double) shouldEqual (nzint.toInt > double)
      }
    }

    it("should offer '>=' comparison that is consistent with Int") {
      forAll { (nzint: NonZeroInt, byte: Byte) =>
        (nzint >= byte) shouldEqual (nzint.toInt >= byte)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        (nzint >= short) shouldEqual (nzint.toInt >= short)
      }
      forAll { (nzint: NonZeroInt, char: Char) =>
        (nzint >= char) shouldEqual (nzint.toInt >= char)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        (nzint >= int) shouldEqual (nzint.toInt >= int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        (nzint >= long) shouldEqual (nzint.toInt >= long)
      }
      forAll { (nzint: NonZeroInt, float: Float) =>
        (nzint >= float) shouldEqual (nzint.toInt >= float)
      }
      forAll { (nzint: NonZeroInt, double: Double) =>
        (nzint >= double) shouldEqual (nzint.toInt >= double)
      }
    }

    it("should offer a '|' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt, byte: Byte) =>
        (nzint | byte) shouldEqual (nzint.toInt | byte)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        (nzint | short) shouldEqual (nzint.toInt | short)
      }
      forAll { (nzint: NonZeroInt, char: Char) =>
        (nzint | char) shouldEqual (nzint.toInt | char)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        (nzint | int) shouldEqual (nzint.toInt | int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        (nzint | long) shouldEqual (nzint.toInt | long)
      }
    }

    it("should offer an '&' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt, byte: Byte) =>
        (nzint & byte) shouldEqual (nzint.toInt & byte)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        (nzint & short) shouldEqual (nzint.toInt & short)
      }
      forAll { (nzint: NonZeroInt, char: Char) =>
        (nzint & char) shouldEqual (nzint.toInt & char)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        (nzint & int) shouldEqual (nzint.toInt & int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        (nzint & long) shouldEqual (nzint.toInt & long)
      }
    }

    it("should offer an '^' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt, byte: Byte) =>
        (nzint ^ byte) shouldEqual (nzint.toInt ^ byte)
      }
      forAll { (nzint: NonZeroInt, char: Char) =>
        (nzint ^ char) shouldEqual (nzint.toInt ^ char)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        (nzint ^ short) shouldEqual (nzint.toInt ^ short)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        (nzint ^ int) shouldEqual (nzint.toInt ^ int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        (nzint ^ long) shouldEqual (nzint.toInt ^ long)
      }
    }

    it("should offer a '+' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt, byte: Byte) =>
        (nzint + byte) shouldEqual (nzint.toInt + byte)
      }
      forAll { (nzint: NonZeroInt, char: Char) =>
        (nzint + char) shouldEqual (nzint.toInt + char)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        (nzint + short) shouldEqual (nzint.toInt + short)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        (nzint + int) shouldEqual (nzint.toInt + int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        (nzint + long) shouldEqual (nzint.toInt + long)
      }
      forAll { (nzint: NonZeroInt, float: Float) =>
        (nzint + float) shouldEqual (nzint.toInt + float)
      }
      forAll { (nzint: NonZeroInt, double: Double) =>
        (nzint + double) shouldEqual (nzint.toInt + double)
      }
    }

    it("should offer a '-' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt, byte: Byte) =>
        (nzint - byte) shouldEqual (nzint.toInt - byte)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        (nzint - short) shouldEqual (nzint.toInt - short)
      }
      forAll { (nzint: NonZeroInt, byte: Char) =>
        (nzint - byte) shouldEqual (nzint.toInt - byte)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        (nzint - int) shouldEqual (nzint.toInt - int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        (nzint - long) shouldEqual (nzint.toInt - long)
      }
      forAll { (nzint: NonZeroInt, float: Float) =>
        (nzint - float) shouldEqual (nzint.toInt - float)
      }
      forAll { (nzint: NonZeroInt, double: Double) =>
        (nzint - double) shouldEqual (nzint.toInt - double)
      }
    }

    it("should offer a '*' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt, byte: Byte) =>
        (nzint * byte) shouldEqual (nzint.toInt * byte)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        (nzint * short) shouldEqual (nzint.toInt * short)
      }
      forAll { (nzint: NonZeroInt, byte: Char) =>
        (nzint * byte) shouldEqual (nzint.toInt * byte)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        (nzint * int) shouldEqual (nzint.toInt * int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        (nzint * long) shouldEqual (nzint.toInt * long)
      }
      forAll { (nzint: NonZeroInt, float: Float) =>
        (nzint * float) shouldEqual (nzint.toInt * float)
      }
      forAll { (nzint: NonZeroInt, double: Double) =>
        (nzint * double) shouldEqual (nzint.toInt * double)
      }
    }

    it("should offer a '/' method that is consistent with Int") {
      // Note that Try (and associated Equality[Try]) are used since some values
      // will legitimately throw an exception

      forAll { (nzint: NonZeroInt, byte: Byte) =>
        Try(nzint / byte) shouldEqual Try(nzint.toInt / byte)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        Try(nzint / short) shouldEqual Try(nzint.toInt / short)
      }
      forAll { (nzint: NonZeroInt, char: Char) =>
        Try(nzint / char) shouldEqual Try(nzint.toInt / char)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        Try(nzint / int) shouldEqual Try(nzint.toInt / int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        Try(nzint / long) shouldEqual Try(nzint.toInt / long)
      }
      forAll { (nzint: NonZeroInt, float: Float) =>
        Try(nzint / float) shouldEqual Try(nzint.toInt / float)
      }
      forAll { (nzint: NonZeroInt, double: Double) =>
        Try(nzint / double) shouldEqual Try(nzint.toInt / double)
      }
    }

    it("should offer a '%' method that is consistent with Int") {
      // Note that Try (and associated Equality[Try]) are used since some values
      // will legitimately throw an exception

      forAll { (nzint: NonZeroInt, byte: Byte) =>
        Try(nzint % byte) shouldEqual Try(nzint.toInt % byte)
      }
      forAll { (nzint: NonZeroInt, short: Short) =>
        Try(nzint % short) shouldEqual Try(nzint.toInt % short)
      }
      forAll { (nzint: NonZeroInt, char: Char) =>
        Try(nzint % char) shouldEqual Try(nzint.toInt % char)
      }
      forAll { (nzint: NonZeroInt, int: Int) =>
        Try(nzint % int) shouldEqual Try(nzint.toInt % int)
      }
      forAll { (nzint: NonZeroInt, long: Long) =>
        Try(nzint % long) shouldEqual Try(nzint.toInt % long)
      }
      forAll { (nzint: NonZeroInt, float: Float) =>
        Try(nzint % float) shouldEqual Try(nzint.toInt % float)
      }
      forAll { (nzint: NonZeroInt, double: Double) =>
        Try(nzint % double) shouldEqual Try(nzint.toInt % double)
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Int") {
      forAll { (nzint1: NonZeroInt, nzint2: NonZeroInt) =>
        nzint1.max(nzint2).toInt shouldEqual nzint1.toInt.max(nzint2.toInt)
        nzint1.min(nzint2).toInt shouldEqual nzint1.toInt.min(nzint2.toInt)
      }
    }

    it("should offer a 'toBinaryString' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt) =>
        nzint.toBinaryString shouldEqual nzint.toInt.toBinaryString
      }
    }

    it("should offer a 'toHexString' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt) =>
        nzint.toHexString shouldEqual nzint.toInt.toHexString
      }
    }

    it("should offer a 'toOctalString' method that is consistent with Int") {
      forAll { (nzint: NonZeroInt) =>
        nzint.toOctalString shouldEqual nzint.toInt.toOctalString
      }
    }

    it("should offer 'to' and 'until' methods that are consistent with Int") {
      forAll { (nzint: NonZeroInt, end: Int, step: Int) =>
        Try(nzint.to(end)) shouldEqual Try(nzint.toInt.to(end))
        Try(nzint.to(end, step)) shouldEqual Try(nzint.toInt.to(end, step))
        Try(nzint.until(end)) shouldEqual Try(nzint.toInt.until(end))
        Try(nzint.until(end, step)) shouldEqual Try(nzint.toInt.until(end, step))
      }
    }

    it("should offer widening methods for basic types that are consistent with Int") {
      forAll { (nzint: NonZeroInt) =>
        def widen(value: Int): Int = value
        widen(nzint) shouldEqual widen(nzint.toInt)
      }
      forAll { (nzint: NonZeroInt) =>
        def widen(value: Long): Long = value
        widen(nzint) shouldEqual widen(nzint.toInt)
      }
      forAll { (nzint: NonZeroInt) =>
        def widen(value: Float): Float = value
        widen(nzint) shouldEqual widen(nzint.toInt)
      }
      forAll { (nzint: NonZeroInt) =>
        def widen(value: Double): Double = value
        widen(nzint) shouldEqual widen(nzint.toInt)
      }
    }
    it("should offer an ensuringValid method that takes an Int => Int, throwing AssertionError if the result is invalid") {
      NonZeroInt(33).ensuringValid(_ + 1) shouldEqual NonZeroInt(34)
      an [AssertionError] should be thrownBy { NonZeroInt(-1).ensuringValid(_ + 1) }
    }
  }
}

