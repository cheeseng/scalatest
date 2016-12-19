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

import org.scalatest._
import OptionValues._
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}
import org.scalactic.Equality
import org.scalatest.prop.GeneratorDrivenPropertyChecks

// SKIP-SCALATESTJS-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS-END
import scala.util.{Failure, Success, Try}


class NonZeroLongSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit def tryEquality[T]: Equality[Try[T]] = new Equality[Try[T]] {
    override def areEqual(a: Try[T], b: Any): Boolean = a match {
      case Success(double: Double) if double.isNaN =>  // This is because in scala.js x/0 results to NaN not ArithmetricException like in jvm, and we need to make sure Success(NaN) == Success(NaN) is true to pass the test.
        b match {
          case Success(bDouble: Double) if bDouble.isNaN => true
          case _ => false
        }
      // I needed this because with GenDrivenPropertyChecks, got:
      // [info] - should offer a '%' method that is consistent with Int *** FAILED ***
      // [info]   Success(NaN) did not equal Success(NaN) (PosIntExperiment.scala:498)
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

  describe("A NonZeroLong") {
    describe("should offer a from factory method that") {
      it("returns Some[NonZeroLong] if the passed Long is greater than 0") {
        NonZeroLong.from(50L).value.value shouldBe 50L
        NonZeroLong.from(100L).value.value shouldBe 100L
      }

      it("returns Some[NonZeroLong] if the passed Long is lesser than 0") {
        NonZeroLong.from(-1L).value.value shouldBe -1L
        NonZeroLong.from(-99L).value.value shouldBe -99L
      }

      it("returns None if the passed Long is 0") {
        NonZeroLong.from(0L) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns Some[NonZeroLong if the passed Long is greater than 0") {
        NonZeroLong.ensuringValid(50L).value shouldBe 50L
        NonZeroLong.ensuringValid(100L).value shouldBe 100L
      }
      it("returns Some[NonZeroLong if the passed Long is lesser than 0") {
        NonZeroLong.ensuringValid(-1L).value shouldBe -1L
        NonZeroLong.ensuringValid(-99L).value shouldBe -99L
      }
      it("throws AssertionError if the passed Long is NOT greater than 0") {
        an [AssertionError] should be thrownBy NonZeroLong.ensuringValid(0L)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Long is not 0") {
        NonZeroLong.isValid(50L) shouldBe true
        NonZeroLong.isValid(100L) shouldBe true
        NonZeroLong.isValid(0L) shouldBe false
        NonZeroLong.isValid(-0L) shouldBe false
        NonZeroLong.isValid(-1L) shouldBe true
        NonZeroLong.isValid(-99L) shouldBe true
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NonZeroLong if the passed Long is greater than 0") {
        NonZeroLong.fromOrElse(50L, NonZeroLong(42L)).value shouldBe 50L
        NonZeroLong.fromOrElse(100L, NonZeroLong(42L)).value shouldBe 100L
      }
      it("returns a NonZeroLong if the passed Long is lesser than 0") {
        NonZeroLong.fromOrElse(-1L, NonZeroLong(42L)).value shouldBe -1L
        NonZeroLong.fromOrElse(-99L, NonZeroLong(42L)).value shouldBe -99L
      }
      it("returns a given default if the passed Long is NOT greater than 0") {
        NonZeroLong.fromOrElse(0L, NonZeroLong(42L)).value shouldBe 42L
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NonZeroLong.MaxValue shouldEqual NonZeroLong.from(Long.MaxValue).get
      NonZeroLong.MinValue shouldEqual NonZeroLong.from(Long.MinValue).get
    }
    it("should have a pretty toString") {
      NonZeroLong.from(42L).value.toString shouldBe "NonZeroLong(42)"
    }
    it("should return the same type from its unary_+ method") {
      +NonZeroLong(3L) shouldEqual NonZeroLong(3L)
    }
    it("should be automatically widened to compatible AnyVal targets") {
      "NonZeroLong(3L): Int" shouldNot typeCheck
      (NonZeroLong(3L): Long) shouldEqual 3L
      (NonZeroLong(3L): Float) shouldEqual 3.0F
      (NonZeroLong(3L): Double) shouldEqual 3.0

      "NonZeroLong(3L): PosInt" shouldNot typeCheck
      "NonZeroLong(3L): PosLong" shouldNot typeCheck
      "NonZeroLong(3L): PosFloat" shouldNot typeCheck
      "NonZeroLong(3L): PosDouble" shouldNot typeCheck

      "NonZeroLong(3L): PosZInt" shouldNot typeCheck
      "NonZeroLong(3L): PosZLong" shouldNot typeCheck
      "NonZeroLong(3L): PosZFloat" shouldNot typeCheck
      "NonZeroLong(3L): PosZLong" shouldNot typeCheck
    }

    it("should be sortable") {
      val xs = List(NonZeroLong(2), NonZeroLong(4), NonZeroLong(1), NonZeroLong(3))
      xs.sorted shouldEqual List(NonZeroLong(1), NonZeroLong(2), NonZeroLong(3), NonZeroLong(4))
    }

    describe("when a compatible AnyVal is passed to a + method invoked on it") {
      it("should give the same AnyVal type back at compile time, and correct value at runtime") {
        // When adding a "primitive"
        val opInt = NonZeroLong(3L) + 3
        opInt shouldEqual 6L

        val opLong = NonZeroLong(3L) + 3L
        opLong shouldEqual 6L

        val opFloat = NonZeroLong(3L) + 3.0F
        opFloat shouldEqual 6.0F

        val opDouble = NonZeroLong(3L) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Pos*
        val opPosInt = NonZeroLong(3L) + PosInt(3)
        opPosInt shouldEqual 6L

        val opPosLong = NonZeroLong(3L) + PosLong(3L)
        opPosLong shouldEqual 6L

        val opPosFloat = NonZeroLong(3L) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0F

        val opPosDouble = NonZeroLong(3L) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *PosZ
        val opPosZ = NonZeroLong(3L) + PosZInt(3)
        opPosZ shouldEqual 6L

        val opPosZLong = NonZeroLong(3L) + PosZLong(3L)
        opPosZLong shouldEqual 6L

        val opPosZFloat = NonZeroLong(3L) + PosZFloat(3.0F)
        opPosZFloat shouldEqual 6.0F

        val opPosZDouble = NonZeroLong(3L) + PosZDouble(3.0)
        opPosZDouble shouldEqual 6.0

        // When adding a NonZero*
        val opNonZeroInt = NonZeroLong(3L) + NonZeroInt(3)
        opNonZeroInt shouldEqual 6

        val opNonZeroLong = NonZeroLong(3L) + NonZeroLong(3L)
        opNonZeroLong shouldEqual 6L

        /*val opNonZeroFloat = NonZeroLong(3L) + NonZeroFloat(3.0F)
        opNonZeroFloat shouldEqual 6.0F

        val opNonZeroDouble = NonZeroLong(3L) + NonZeroDouble(3.0)
        opNonZeroDouble shouldEqual 6.0*/
      }
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "NonZeroLong(8)" should compile
        NonZeroLong(8).value shouldEqual 8L
        "NonZeroLong(8L)" should compile
        NonZeroLong(8L).value shouldEqual 8L
      }

      it("should not compile when 0 is passed in") {
        "NonZeroLong(0)" shouldNot compile
        "NonZeroLong(0L)" shouldNot compile
      }

      it("should compile when -8 is passed in") {
        "NonZeroLong(-8)" should compile
        NonZeroLong(-8).value shouldEqual -8L
        "NonZeroLong(-8L)" should compile
        NonZeroLong(-8L).value shouldEqual -8L
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "NonZeroLong(a)" shouldNot compile
        val b: Long = -8L
        "NonZeroLong(b)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Long") {

      def takesNonZeroLong(pos: NonZeroLong): Long = pos.value

      it("should compile when 8 is passed in") {
        "takesNonZeroLong(8)" should compile
        takesNonZeroLong(8) shouldEqual 8L
        "takesNonZeroLong(8L)" should compile
        takesNonZeroLong(8L) shouldEqual 8L
      }

      it("should not compile when 0 is passed in") {
        "takesNonZeroLong(0)" shouldNot compile
        "takesNonZeroLong(0L)" shouldNot compile
      }

      it("should not compile when -8 is passed in") {
        "takesNonZeroLong(-8)" shouldNot compile
        "takesNonZeroLong(-8L)" shouldNot compile
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNonZeroLong(x)" shouldNot compile
        val b: Long = -8L
        "takesNonZeroLong(b)" shouldNot compile
      }

      it("should offer a unary ~ method that is consistent with Long") {
        forAll { (plong: NonZeroLong) =>
          (~plong) shouldEqual (~(plong.toLong))
        }
      }

      it("should offer a unary + method that is consistent with Long") {
        forAll { (plong: NonZeroLong) =>
          (+plong).toLong shouldEqual (+(plong.toLong))
        }
      }

      it("should offer a unary - method that is consistent with Long") {
        forAll { (plong: NonZeroLong) =>
          (-plong) shouldEqual (-(plong.toLong))
        }
      }

      it("should offer << methods that are consistent with Long") {
        forAll { (plong: NonZeroLong, shift: Int) =>
          plong << shift shouldEqual plong.toLong << shift
        }
        forAll { (plong: NonZeroLong, shift: Long) =>
          plong << shift shouldEqual plong.toLong << shift
        }
      }

      it("should offer >>> methods that are consistent with Long") {
        forAll { (plong: NonZeroLong, shift: Int) =>
          plong >>> shift shouldEqual plong.toLong >>> shift
        }
        forAll { (plong: NonZeroLong, shift: Long) =>
          plong >>> shift shouldEqual plong.toLong >>> shift
        }
      }

      it("should offer >> methods that are consistent with Long") {
        forAll { (plong: NonZeroLong, shift: Int) =>
          plong >> shift shouldEqual plong.toLong >> shift
        }
        forAll { (plong: NonZeroLong, shift: Long) =>
          plong >> shift shouldEqual plong.toLong >> shift
        }
      }

      it("should offer '<' comparison that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          (plong < byte) shouldEqual (plong.toLong < byte)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          (plong < short) shouldEqual (plong.toLong < short)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          (plong < char) shouldEqual (plong.toLong < char)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          (plong < int) shouldEqual (plong.toLong < int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          (plong < long) shouldEqual (plong.toLong < long)
        }
        forAll { (plong: NonZeroLong, float: Float) =>
          (plong < float) shouldEqual (plong.toLong < float)
        }
        forAll { (plong: NonZeroLong, double: Double) =>
          (plong < double) shouldEqual (plong.toLong < double)
        }
      }

      it("should offer '<=' comparison that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          (plong <= byte) shouldEqual (plong.toLong <= byte)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          (plong <= char) shouldEqual (plong.toLong <= char)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          (plong <= short) shouldEqual (plong.toLong <= short)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          (plong <= int) shouldEqual (plong.toLong <= int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          (plong <= long) shouldEqual (plong.toLong <= long)
        }
        forAll { (plong: NonZeroLong, float: Float) =>
          (plong <= float) shouldEqual (plong.toLong <= float)
        }
        forAll { (plong: NonZeroLong, double: Double) =>
          (plong <= double) shouldEqual (plong.toLong <= double)
        }
      }

      it("should offer '>' comparison that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          (plong > byte) shouldEqual (plong.toLong > byte)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          (plong > short) shouldEqual (plong.toLong > short)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          (plong > char) shouldEqual (plong.toLong > char)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          (plong > int) shouldEqual (plong.toLong > int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          (plong > long) shouldEqual (plong.toLong > long)
        }
        forAll { (plong: NonZeroLong, float: Float) =>
          (plong > float) shouldEqual (plong.toLong > float)
        }
        forAll { (plong: NonZeroLong, double: Double) =>
          (plong > double) shouldEqual (plong.toLong > double)
        }
      }

      it("should offer '>=' comparison that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          (plong >= byte) shouldEqual (plong.toLong >= byte)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          (plong >= short) shouldEqual (plong.toLong >= short)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          (plong >= char) shouldEqual (plong.toLong >= char)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          (plong >= int) shouldEqual (plong.toLong >= int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          (plong >= long) shouldEqual (plong.toLong >= long)
        }
        forAll { (plong: NonZeroLong, float: Float) =>
          (plong >= float) shouldEqual (plong.toLong >= float)
        }
        forAll { (plong: NonZeroLong, double: Double) =>
          (plong >= double) shouldEqual (plong.toLong >= double)
        }
      }

      it("should offer a '|' method that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          (plong | byte) shouldEqual (plong.toLong | byte)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          (plong | short) shouldEqual (plong.toLong | short)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          (plong | char) shouldEqual (plong.toLong | char)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          (plong | int) shouldEqual (plong.toLong | int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          (plong | long) shouldEqual (plong.toLong | long)
        }
      }

      it("should offer an '&' method that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          (plong & byte) shouldEqual (plong.toLong & byte)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          (plong & short) shouldEqual (plong.toLong & short)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          (plong & char) shouldEqual (plong.toLong & char)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          (plong & int) shouldEqual (plong.toLong & int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          (plong & long) shouldEqual (plong.toLong & long)
        }
      }

      it("should offer an '^' method that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          (plong ^ byte) shouldEqual (plong.toLong ^ byte)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          (plong ^ short) shouldEqual (plong.toLong ^ short)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          (plong ^ char) shouldEqual (plong.toLong ^ char)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          (plong ^ int) shouldEqual (plong.toLong ^ int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          (plong ^ long) shouldEqual (plong.toLong ^ long)
        }
      }

      it("should offer a '+' method that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          (plong + byte) shouldEqual (plong.toLong + byte)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          (plong + short) shouldEqual (plong.toLong + short)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          (plong + char) shouldEqual (plong.toLong + char)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          (plong + int) shouldEqual (plong.toLong + int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          (plong + long) shouldEqual (plong.toLong + long)
        }
        forAll { (plong: NonZeroLong, float: Float) =>
          (plong + float) shouldEqual (plong.toLong + float)
        }
        forAll { (plong: NonZeroLong, double: Double) =>
          (plong + double) shouldEqual (plong.toLong + double)
        }
      }

      it("should offer a '-' method that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          (plong - byte) shouldEqual (plong.toLong - byte)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          (plong - short) shouldEqual (plong.toLong - short)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          (plong - char) shouldEqual (plong.toLong - char)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          (plong - int) shouldEqual (plong.toLong - int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          (plong - long) shouldEqual (plong.toLong - long)
        }
        forAll { (plong: NonZeroLong, float: Float) =>
          (plong - float) shouldEqual (plong.toLong - float)
        }
        forAll { (plong: NonZeroLong, double: Double) =>
          (plong - double) shouldEqual (plong.toLong - double)
        }
      }

      it("should offer a '*' method that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          (plong * byte) shouldEqual (plong.toLong * byte)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          (plong * short) shouldEqual (plong.toLong * short)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          (plong * char) shouldEqual (plong.toLong * char)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          (plong * int) shouldEqual (plong.toLong * int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          (plong * long) shouldEqual (plong.toLong * long)
        }
        forAll { (plong: NonZeroLong, float: Float) =>
          (plong * float) shouldEqual (plong.toLong * float)
        }
        forAll { (plong: NonZeroLong, double: Double) =>
          (plong * double) shouldEqual (plong.toLong * double)
        }
      }

      it("should offer a '/' method that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          Try(plong / byte) shouldEqual Try(plong.toLong / byte)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          Try(plong / short) shouldEqual Try(plong.toLong / short)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          Try(plong / char) shouldEqual Try(plong.toLong / char)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          Try(plong / int) shouldEqual Try(plong.toLong / int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          Try(plong / long) shouldEqual Try(plong.toLong / long)
        }
        forAll { (plong: NonZeroLong, float: Float) =>
          Try(plong / float) shouldEqual Try(plong.toLong / float)
        }
        forAll { (plong: NonZeroLong, double: Double) =>
          Try(plong / double) shouldEqual Try(plong.toLong / double)
        }
      }

      it("should offer a '%' method that is consistent with Long") {
        forAll { (plong: NonZeroLong, byte: Byte) =>
          Try(plong % byte) shouldEqual Try(plong.toLong % byte)
        }
        forAll { (plong: NonZeroLong, short: Short) =>
          Try(plong % short) shouldEqual Try(plong.toLong % short)
        }
        forAll { (plong: NonZeroLong, char: Char) =>
          Try(plong % char) shouldEqual Try(plong.toLong % char)
        }
        forAll { (plong: NonZeroLong, int: Int) =>
          Try(plong % int) shouldEqual Try(plong.toLong % int)
        }
        forAll { (plong: NonZeroLong, long: Long) =>
          Try(plong % long) shouldEqual Try(plong.toLong % long)
        }
        forAll { (plong: NonZeroLong, float: Float) =>
          Try(plong % float) shouldEqual Try(plong.toLong % float)
        }
        forAll { (plong: NonZeroLong, double: Double) =>
          Try(plong % double) shouldEqual Try(plong.toLong % double)
        }
      }

      it("should offer 'min' and 'max' methods that are consistent with Long") {
        forAll { (plong1: NonZeroLong, plong2: NonZeroLong) =>
          plong1.max(plong2).toLong shouldEqual plong1.toLong.max(plong2.toLong)
          plong1.min(plong2).toLong shouldEqual plong1.toLong.min(plong2.toLong)
        }
      }

      it("should offer a 'toBinaryString' method that is consistent with Long") {
        forAll { (plong: NonZeroLong) =>
          plong.toBinaryString shouldEqual plong.toLong.toBinaryString
        }
      }

      it("should offer a 'toHexString' method that is consistent with Long") {
        forAll { (plong: NonZeroLong) =>
          plong.toHexString shouldEqual plong.toLong.toHexString
        }
      }

      it("should offer a 'toOctalString' method that is consistent with Long") {
        forAll { (plong: NonZeroLong) =>
          plong.toOctalString shouldEqual plong.toLong.toOctalString
        }
      }

      // SKIP-SCALATESTJS-START
      it("should offer 'to' and 'until' method that is consistent with Long") {
        def rangeEqual[T](a: NumericRange[T], b: NumericRange[T]): Boolean =
          a.start == b.start && a.end == b.end && a.step == b.step

        forAll { (plong: NonZeroLong, end: Long, step: Long) =>
          rangeEqual(plong.until(end), plong.toLong.until(end)) shouldBe true
          rangeEqual(plong.until(end, step), plong.toLong.until(end, step)) shouldBe true
          rangeEqual(plong.to(end), plong.toLong.to(end)) shouldBe true
          rangeEqual(plong.to(end, step), plong.toLong.to(end, step)) shouldBe true
        }
      }
      // SKIP-SCALATESTJS-END

      it("should offer widening methods for basic types that are consistent with Long") {
        forAll { (plong: NonZeroLong) =>
          def widen(value: Long): Long = value
          widen(plong) shouldEqual widen(plong.toLong)
        }
        forAll { (plong: NonZeroLong) =>
          def widen(value: Float): Float = value
          widen(plong) shouldEqual widen(plong.toLong)
        }
        forAll { (plong: NonZeroLong) =>
          def widen(value: Double): Double = value
          widen(plong) shouldEqual widen(plong.toLong)
        }
        /*forAll { (plong: NonZeroLong) =>
          def widen(value: NonZeroFloat): NonZeroFloat = value
          widen(plong) shouldEqual widen(NonZeroFloat.from(plong.toLong).get)
        }
        forAll { (plong: NonZeroLong) =>
          def widen(value: NonZeroDouble): NonZeroDouble = value
          widen(plong) shouldEqual widen(NonZeroDouble.from(plong.toLong).get)
        }*/
      }
    }
    it("should offer an ensuringValid method that takes an Long => Long, throwing AssertionError if the result is invalid") {
      NonZeroLong(33L).ensuringValid(_ + 1L) shouldEqual NonZeroLong(34L)
      an [AssertionError] should be thrownBy { NonZeroLong(-1L).ensuringValid(_ + 1L) }
    }
  }
}