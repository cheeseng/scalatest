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
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.PropertyChecks
// SKIP-SCALATESTJS-START
import scala.collection.immutable.NumericRange
// SKIP-SCALATESTJS-END
import scala.util.{Failure, Success, Try}

class NonZeroFloatSpec extends FunSpec with Matchers with PropertyChecks with TypeCheckedTripleEquals {

  val posFloatGen: Gen[NonZeroFloat] =
    for {i <- choose(1, Float.MaxValue)} yield NonZeroFloat.ensuringValid(i)

  describe("A NonZeroFloat") {
    describe("should offer a from factory method that") {
      it("returns Some[NonZeroFloat] if the passed Float is greater than 0") {
        NonZeroFloat.from(50.23F).value.value shouldBe 50.23F
        NonZeroFloat.from(100.0F).value.value shouldBe 100.0F
      }
      it("returns Some[NonZeroFloat] if the passed Float is lesser than 0") {
        NonZeroFloat.from(-0.00001F).value.value shouldBe -0.00001F
        NonZeroFloat.from(-99.9F).value.value shouldBe -99.9F
      }
      it("returns None if the passed Float is  0") {
        NonZeroFloat.from(0.0F) shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns NonZeroFloat if the passed Float is greater than 0") {
        NonZeroFloat.ensuringValid(50.23F).value shouldBe 50.23F
        NonZeroFloat.ensuringValid(100.0F).value shouldBe 100.0F
        NonZeroFloat.ensuringValid(Float.PositiveInfinity).value shouldBe Float.PositiveInfinity
      }
      it("returns NonZeroFloat if the passed Float is lesser than 0") {
        NonZeroFloat.ensuringValid(-0.00001F).value shouldBe -0.00001F
        NonZeroFloat.ensuringValid(-99.9F).value shouldBe -99.9F
        NonZeroFloat.ensuringValid(Float.NegativeInfinity).value shouldBe Float.NegativeInfinity
      }
      it("throws AssertionError if the passed Float is NaN") {
        an [AssertionError] should be thrownBy NonZeroFloat.ensuringValid(Float.NaN)
      }
      it("throws AssertionError if the passed Float is 0") {
        an [AssertionError] should be thrownBy NonZeroFloat.ensuringValid(0.0F)
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Float is non-zero") {
        NonZeroFloat.isValid(50.23f) shouldBe true
        NonZeroFloat.isValid(100.0f) shouldBe true
        NonZeroFloat.isValid(0.0f) shouldBe false
        NonZeroFloat.isValid(-0.0f) shouldBe false
        NonZeroFloat.isValid(-0.00001f) shouldBe true
        NonZeroFloat.isValid(-99.9f) shouldBe true
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a NonZeroFloat if the passed Float is greater than 0") {
        NonZeroFloat.fromOrElse(50.23f, NonZeroFloat(42.0f)).value shouldBe 50.23f
        NonZeroFloat.fromOrElse(100.0f, NonZeroFloat(42.0f)).value shouldBe 100.0f
      }
      it("returns a NonZeroFloat if the passed Float is lesser than 0") {
        NonZeroFloat.fromOrElse(-0.00001f, NonZeroFloat(42.0f)).value shouldBe -0.00001f
        NonZeroFloat.fromOrElse(-99.9f, NonZeroFloat(42.0f)).value shouldBe -99.9f
      }
      it("returns a given default if the passed Float is 0") {
        NonZeroFloat.fromOrElse(0.0f, NonZeroFloat(42.0f)).value shouldBe 42.0f
      }
    }
    it("should offer MaxValue and MinValue factory methods") {
      NonZeroFloat.MaxValue shouldEqual NonZeroFloat.from(Float.MaxValue).get
      NonZeroFloat.MinValue shouldEqual
        NonZeroFloat.from(Float.MinValue).get
    }
    it("should offer a PositiveInfinity factory method") {
      NonZeroFloat.PositiveInfinity shouldEqual NonZeroFloat.ensuringValid(Float.PositiveInfinity)
    }
    it("should offer a NegativeInfinity factory method") {
      NonZeroFloat.NegativeInfinity shouldEqual NonZeroFloat.ensuringValid(Float.NegativeInfinity)
    }
    it("should have a pretty toString") {
      // SKIP-SCALATESTJS-START
      NonZeroFloat.from(42.0F).value.toString shouldBe "NonZeroFloat(42.0)"
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY NonZeroFloat.from(42.0F).value.toString shouldBe "NonZeroFloat(42)"
    }
    it("should return the same type from its unary_+ method") {
      +NonZeroFloat(3.0F) shouldEqual NonZeroFloat(3.0F)
    }
    it("should be automatically widened to compatible AnyVal targets") {
      "NonZeroFloat(3.0F): Int" shouldNot typeCheck
      "NonZeroFloat(3.0F): Long" shouldNot typeCheck
      (NonZeroFloat(3.0F): Float) shouldEqual 3.0F
      (NonZeroFloat(3.0F): Double) shouldEqual 3.0

      "NonZeroFloat(3.0F): PosInt" shouldNot typeCheck
      "NonZeroFloat(3.0F): PosLong" shouldNot typeCheck
      "NonZeroFloat(3.0F): PosFloat" shouldNot typeCheck
      "NonZeroFloat(3.0F): PosDouble" shouldNot typeCheck

      "NonZeroFloat(3.0F): PosZInt" shouldNot typeCheck
      "NonZeroFloat(3.0F): PosZLong" shouldNot typeCheck
      "NonZeroFloat(3.0F): PosZFloat" shouldNot typeCheck
      "NonZeroFloat(3.0F): PosZDouble" shouldNot typeCheck

      "NonZeroFloat(3.0F): NonZeroZInt" shouldNot typeCheck
      "NonZeroFloat(3.0F): NonZeroZLong" shouldNot typeCheck
    }

    it("should be sortable") {
      val xs = List(NonZeroFloat(2.2F), NonZeroFloat(4.4F), NonZeroFloat(1.1F),
        NonZeroFloat(3.3F))
      xs.sorted shouldEqual List(NonZeroFloat(1.1F), NonZeroFloat(2.2F), NonZeroFloat(3.3F),
        NonZeroFloat(4.4F))
    }

    describe("when a compatible AnyVal is passed to a + method invoked on it") {
      it("should give the same AnyVal type back at compile time, and correct value at runtime") {
        // When adding a "primitive"
        val opInt = NonZeroFloat(3.0F) + 3
        opInt shouldEqual 6.0F

        val opLong = NonZeroFloat(3.0F) + 3L
        opLong shouldEqual 6.0F

        val opFloat = NonZeroFloat(3.0F) + 3.0F
        opFloat shouldEqual 6.0F

        val opDouble = NonZeroFloat(3.0F) + 3.0
        opDouble shouldEqual 6.0

        // When adding a Pos*
        val opPosInt = NonZeroFloat(3.0F) + PosInt(3)
        opPosInt shouldEqual 6.0F

        val opPosLong = NonZeroFloat(3.0F) + PosLong(3L)
        opPosLong shouldEqual 6.0F

        val opPosFloat = NonZeroFloat(3.0F) + PosFloat(3.0F)
        opPosFloat shouldEqual 6.0F

        val opPosDouble = NonZeroFloat(3.0F) + PosDouble(3.0)
        opPosDouble shouldEqual 6.0

        // When adding a *PosZ
        val opPosZ = NonZeroFloat(3.0F) + PosZInt(3)
        opPosZ shouldEqual 6.0F

        val opPosZLong = NonZeroFloat(3.0F) + PosZLong(3L)
        opPosZLong shouldEqual 6.0F

        val opPosZFloat = NonZeroFloat(3.0F) + PosZFloat(3.0F)
        opPosZFloat shouldEqual 6.0F

        val opPosZDouble = NonZeroFloat(3.0F) + PosZDouble(3.0)
        opPosZDouble shouldEqual 6.0

        // When adding a NonZero*
        val opNonZeroInt = NonZeroFloat(3.0F) + NonZeroInt(3)
        opNonZeroInt shouldEqual 6

        val opNonZeroLong = NonZeroFloat(3.0F) + NonZeroLong(3L)
        opNonZeroLong shouldEqual 6L

        val opNonZeroFloat = NonZeroFloat(3.0F) + NonZeroFloat(3.0F)
        opNonZeroFloat shouldEqual 6.0F

        /*val opNonZeroDouble = NonZeroFloat(3.0F) + NonZeroDouble(3.0)
        opNonZeroDouble shouldEqual 6.0*/
      }
    }

    describe("when created with apply method") {

      it("should compile when 8 is passed in") {
        "NonZeroFloat(8)" should compile
        NonZeroFloat(8).value shouldEqual 8.0F
        "NonZeroFloat(8L)" should compile
        NonZeroFloat(8L).value shouldEqual 8.0F
        "NonZeroFloat(8.0F)" should compile
        NonZeroFloat(8.0F).value shouldEqual 8.0F
      }

      it("should not compile when 0 is passed in") {
        "NonZeroFloat(0)" shouldNot compile
        "NonZeroFloat(0L)" shouldNot compile
        "NonZeroFloat(0.0F)" shouldNot compile
      }


      it("should compile when -8 is passed in") {
        "NonZeroFloat(-8)" should compile
        NonZeroFloat(-8).value shouldEqual -8.0F
        "NonZeroFloat(-8L)" should compile
        NonZeroFloat(-8L).value shouldEqual -8.0F
        "NonZeroFloat(-8.0F)" should compile
        NonZeroFloat(-8.0F).value shouldEqual -8.0F
      }

      it("should not compile when x is passed in") {
        val a: Int = -8
        "NonZeroFloat(a)" shouldNot compile
        val b: Long = -8L
        "NonZeroFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "NonZeroFloat(c)" shouldNot compile
      }
    }
    describe("when specified as a plain-old Float") {

      def takesNonZeroFloat(pos: NonZeroFloat): Float = pos.value

      it("should compile when 8 is passed in") {
        "takesNonZeroFloat(8)" should compile
        takesNonZeroFloat(8) shouldEqual 8.0F
        "takesNonZeroFloat(8L)" should compile
        takesNonZeroFloat(8L) shouldEqual 8.0F
        "takesNonZeroFloat(8.0F)" should compile
        takesNonZeroFloat(8.0F) shouldEqual 8.0F
      }

      it("should not compile when 0 is passed in") {
        "takesNonZeroFloat(0)" shouldNot compile
        "takesNonZeroFloat(0L)" shouldNot compile
        "takesNonZeroFloat(0.0F)" shouldNot compile
      }

      it("should compile when -8 is passed in") {
        "takesNonZeroFloat(-8)" should compile
        takesNonZeroFloat(-8) shouldEqual -8.0F
        "takesNonZeroFloat(-8L)" should compile
        takesNonZeroFloat(-8L) shouldEqual -8.0F
        "takesNonZeroFloat(-8.0F)" should compile
        takesNonZeroFloat(-8.0F) shouldEqual -8.0F
      }

      it("should not compile when x is passed in") {
        val x: Int = -8
        "takesNonZeroFloat(x)" shouldNot compile
        val b: Long = -8L
        "takesNonZeroFloat(b)" shouldNot compile
        val c: Float = -8.0F
        "takesNonZeroFloat(c)" shouldNot compile
      }
    }

    it("should offer a unary + method that is consistent with Float") {
      forAll { (pfloat: NonZeroFloat) =>
        (+pfloat).toFloat shouldEqual (+(pfloat.toFloat))
      }
    }

    it("should offer a unary - method that is consistent with Float") {
      forAll { (pfloat: NonZeroFloat) =>
        (-pfloat) shouldEqual (-(pfloat.toFloat))
      }
    }

    it("should offer '<' comparison that is consistent with Float") {
      forAll { (pfloat: NonZeroFloat, byte: Byte) =>
        (pfloat < byte) shouldEqual (pfloat.toFloat < byte)
      }
      forAll { (pfloat: NonZeroFloat, short: Short) =>
        (pfloat < short) shouldEqual (pfloat.toFloat < short)
      }
      forAll { (pfloat: NonZeroFloat, char: Char) =>
        (pfloat < char) shouldEqual (pfloat.toFloat < char)
      }
      forAll { (pfloat: NonZeroFloat, int: Int) =>
        (pfloat < int) shouldEqual (pfloat.toFloat < int)
      }
      forAll { (pfloat: NonZeroFloat, long: Long) =>
        (pfloat < long) shouldEqual (pfloat.toFloat < long)
      }
      forAll { (pfloat: NonZeroFloat, float: Float) =>
        (pfloat < float) shouldEqual (pfloat.toFloat < float)
      }
      forAll { (pfloat: NonZeroFloat, double: Double) =>
        (pfloat < double) shouldEqual (pfloat.toFloat < double)
      }
    }

    it("should offer '<=' comparison that is consistent with Float") {
      forAll { (pfloat: NonZeroFloat, byte: Byte) =>
        (pfloat <= byte) shouldEqual (pfloat.toFloat <= byte)
      }
      forAll { (pfloat: NonZeroFloat, char: Char) =>
        (pfloat <= char) shouldEqual (pfloat.toFloat <= char)
      }
      forAll { (pfloat: NonZeroFloat, short: Short) =>
        (pfloat <= short) shouldEqual (pfloat.toFloat <= short)
      }
      forAll { (pfloat: NonZeroFloat, int: Int) =>
        (pfloat <= int) shouldEqual (pfloat.toFloat <= int)
      }
      forAll { (pfloat: NonZeroFloat, long: Long) =>
        (pfloat <= long) shouldEqual (pfloat.toFloat <= long)
      }
      forAll { (pfloat: NonZeroFloat, float: Float) =>
        (pfloat <= float) shouldEqual (pfloat.toFloat <= float)
      }
      forAll { (pfloat: NonZeroFloat, double: Double) =>
        (pfloat <= double) shouldEqual (pfloat.toFloat <= double)
      }
    }

    it("should offer '>' comparison that is consistent with Float") {
      forAll { (pfloat: NonZeroFloat, byte: Byte) =>
        (pfloat > byte) shouldEqual (pfloat.toFloat > byte)
      }
      forAll { (pfloat: NonZeroFloat, short: Short) =>
        (pfloat > short) shouldEqual (pfloat.toFloat > short)
      }
      forAll { (pfloat: NonZeroFloat, char: Char) =>
        (pfloat > char) shouldEqual (pfloat.toFloat > char)
      }
      forAll { (pfloat: NonZeroFloat, int: Int) =>
        (pfloat > int) shouldEqual (pfloat.toFloat > int)
      }
      forAll { (pfloat: NonZeroFloat, long: Long) =>
        (pfloat > long) shouldEqual (pfloat.toFloat > long)
      }
      forAll { (pfloat: NonZeroFloat, float: Float) =>
        (pfloat > float) shouldEqual (pfloat.toFloat > float)
      }
      forAll { (pfloat: NonZeroFloat, double: Double) =>
        (pfloat > double) shouldEqual (pfloat.toFloat > double)
      }
    }

    it("should offer '>=' comparison that is consistent with Float") {
      forAll { (pfloat: NonZeroFloat, byte: Byte) =>
        (pfloat >= byte) shouldEqual (pfloat.toFloat >= byte)
      }
      forAll { (pfloat: NonZeroFloat, short: Short) =>
        (pfloat >= short) shouldEqual (pfloat.toFloat >= short)
      }
      forAll { (pfloat: NonZeroFloat, char: Char) =>
        (pfloat >= char) shouldEqual (pfloat.toFloat >= char)
      }
      forAll { (pfloat: NonZeroFloat, int: Int) =>
        (pfloat >= int) shouldEqual (pfloat.toFloat >= int)
      }
      forAll { (pfloat: NonZeroFloat, long: Long) =>
        (pfloat >= long) shouldEqual (pfloat.toFloat >= long)
      }
      forAll { (pfloat: NonZeroFloat, float: Float) =>
        (pfloat >= float) shouldEqual (pfloat.toFloat >= float)
      }
      forAll { (pfloat: NonZeroFloat, double: Double) =>
        (pfloat >= double) shouldEqual (pfloat.toFloat >= double)
      }
    }

    it("should offer a '+' method that is consistent with Float") {
      forAll { (pfloat: NonZeroFloat, byte: Byte) =>
        (pfloat + byte) shouldEqual (pfloat.toFloat + byte)
      }
      forAll { (pfloat: NonZeroFloat, short: Short) =>
        (pfloat + short) shouldEqual (pfloat.toFloat + short)
      }
      forAll { (pfloat: NonZeroFloat, char: Char) =>
        (pfloat + char) shouldEqual (pfloat.toFloat + char)
      }
      forAll { (pfloat: NonZeroFloat, int: Int) =>
        (pfloat + int) shouldEqual (pfloat.toFloat + int)
      }
      forAll { (pfloat: NonZeroFloat, long: Long) =>
        (pfloat + long) shouldEqual (pfloat.toFloat + long)
      }
      forAll { (pfloat: NonZeroFloat, float: Float) =>
        (pfloat + float) shouldEqual (pfloat.toFloat + float)
      }
      forAll { (pfloat: NonZeroFloat, double: Double) =>
        (pfloat + double) shouldEqual (pfloat.toFloat + double)
      }
    }

    it("should offer a '-' method that is consistent with Float") {
      forAll { (pfloat: NonZeroFloat, byte: Byte) =>
        (pfloat - byte) shouldEqual (pfloat.toFloat - byte)
      }
      forAll { (pfloat: NonZeroFloat, short: Short) =>
        (pfloat - short) shouldEqual (pfloat.toFloat - short)
      }
      forAll { (pfloat: NonZeroFloat, char: Char) =>
        (pfloat - char) shouldEqual (pfloat.toFloat - char)
      }
      forAll { (pfloat: NonZeroFloat, int: Int) =>
        (pfloat - int) shouldEqual (pfloat.toFloat - int)
      }
      forAll { (pfloat: NonZeroFloat, long: Long) =>
        (pfloat - long) shouldEqual (pfloat.toFloat - long)
      }
      forAll { (pfloat: NonZeroFloat, float: Float) =>
        (pfloat - float) shouldEqual (pfloat.toFloat - float)
      }
      forAll { (pfloat: NonZeroFloat, double: Double) =>
        (pfloat - double) shouldEqual (pfloat.toFloat - double)
      }
    }

    it("should offer a '*' method that is consistent with Float") {
      forAll { (pfloat: NonZeroFloat, byte: Byte) =>
        (pfloat * byte) shouldEqual (pfloat.toFloat * byte)
      }
      forAll { (pfloat: NonZeroFloat, short: Short) =>
        (pfloat * short) shouldEqual (pfloat.toFloat * short)
      }
      forAll { (pfloat: NonZeroFloat, char: Char) =>
        (pfloat * char) shouldEqual (pfloat.toFloat * char)
      }
      forAll { (pfloat: NonZeroFloat, int: Int) =>
        (pfloat * int) shouldEqual (pfloat.toFloat * int)
      }
      forAll { (pfloat: NonZeroFloat, long: Long) =>
        (pfloat * long) shouldEqual (pfloat.toFloat * long)
      }
      forAll { (pfloat: NonZeroFloat, float: Float) =>
        (pfloat * float) shouldEqual (pfloat.toFloat * float)
      }
      forAll { (pfloat: NonZeroFloat, double: Double) =>
        (pfloat * double) shouldEqual (pfloat.toFloat * double)
      }
    }

    it("should offer a '/' method that is consistent with Float") {
      forAll { (pfloat: NonZeroFloat, byte: Byte) =>
        pfloat / byte shouldEqual pfloat.toFloat / byte
      }
      forAll { (pfloat: NonZeroFloat, short: Short) =>
        pfloat / short shouldEqual pfloat.toFloat / short
      }
      forAll { (pfloat: NonZeroFloat, char: Char) =>
        pfloat / char shouldEqual pfloat.toFloat / char
      }
      forAll { (pfloat: NonZeroFloat, int: Int) =>
        pfloat / int shouldEqual pfloat.toFloat / int
      }
      forAll { (pfloat: NonZeroFloat, long: Long) =>
        pfloat / long shouldEqual pfloat.toFloat / long
      }
      forAll { (pfloat: NonZeroFloat, float: Float) =>
        pfloat / float shouldEqual pfloat.toFloat / float
      }
      forAll { (pfloat: NonZeroFloat, double: Double) =>
        pfloat / double shouldEqual pfloat.toFloat / double
      }
    }

    // note: since a PosInt % 0 is NaN (as opposed to PosInt / 0, which is Infinity)
    // extra logic is needed to convert to a comparable type (boolean, in this case)
    it("should offer a '%' method that is consistent with Float") {
      forAll { (pfloat: NonZeroFloat, byte: Byte) =>
        val res = pfloat % byte
        if (res.isNaN)
          (pfloat.toFloat % byte).isNaN shouldBe true
        else
          res shouldEqual pfloat.toFloat % byte
      }
      forAll { (pfloat: NonZeroFloat, short: Short) =>
        val res = pfloat % short
        if (res.isNaN)
          (pfloat.toFloat % short).isNaN shouldBe true
        else
          res shouldEqual pfloat.toFloat % short
      }
      forAll { (pfloat: NonZeroFloat, char: Char) =>
        val res = pfloat % char
        if (res.isNaN)
          (pfloat.toFloat % char).isNaN shouldBe true
        else
          res shouldEqual pfloat.toFloat % char
      }
      forAll { (pfloat: NonZeroFloat, int: Int) =>
        val res = pfloat % int
        if (res.isNaN)
          (pfloat.toFloat % int).isNaN shouldBe true
        else
          res shouldEqual pfloat.toFloat % int
      }
      forAll { (pfloat: NonZeroFloat, long: Long) =>
        val res = pfloat % long
        if (res.isNaN)
          (pfloat.toFloat % long).isNaN shouldBe true
        else
          res shouldEqual pfloat.toFloat % long
      }
      forAll { (pfloat: NonZeroFloat, float: Float) =>
        val res = pfloat % float
        if (res.isNaN)
          (pfloat.toFloat % float).isNaN shouldBe true
        else
          res shouldEqual pfloat.toFloat % float
      }
      forAll { (pfloat: NonZeroFloat, double: Double) =>
        val res = pfloat % double
        if (res.isNaN)
          (pfloat.toFloat % double).isNaN shouldBe true
        else
          res shouldEqual pfloat.toFloat % double
      }
    }

    it("should offer 'min' and 'max' methods that are consistent with Float") {
      forAll { (pfloat1: NonZeroFloat, pfloat2: NonZeroFloat) =>
        pfloat1.max(pfloat2).toFloat shouldEqual pfloat1.toFloat.max(pfloat2.toFloat)
        pfloat1.min(pfloat2).toFloat shouldEqual pfloat1.toFloat.min(pfloat2.toFloat)
      }
    }

    it("should offer an 'isWhole' method that is consistent with Float") {
      forAll { (pfloat: NonZeroFloat) =>
        pfloat.isWhole shouldEqual pfloat.toFloat.isWhole
      }
    }

    it("should offer 'toRadians' and 'toDegrees' methods that are consistent with Float") {
      forAll { (pfloat: NonZeroFloat) =>
        pfloat.toRadians shouldEqual pfloat.toFloat.toRadians
      }
    }

    // SKIP-SCALATESTJS-START
    it("should offer 'to' and 'until' method that is consistent with Float") {
      def rangeEqual[T](a: NumericRange[T], b: NumericRange[T]): Boolean =
        a.start == b.start && a.end == b.end && a.step == b.step

      forAll { (pfloat: NonZeroFloat, end: Float, step: Float) =>
        rangeEqual(pfloat.until(end).by(1f), pfloat.toFloat.until(end).by(1f)) shouldBe true
        rangeEqual(pfloat.until(end, step), pfloat.toFloat.until(end, step)) shouldBe true
        rangeEqual(pfloat.to(end).by(1f), pfloat.toFloat.to(end).by(1f)) shouldBe true
        rangeEqual(pfloat.to(end, step), pfloat.toFloat.to(end, step)) shouldBe true
      }
    }
    // SKIP-SCALATESTJS-END

    it("should offer widening methods for basic types that are consistent with Float") {
      forAll { (pfloat: NonZeroFloat) =>
        def widen(value: Float): Float = value
        widen(pfloat) shouldEqual widen(pfloat.toFloat)
      }
      forAll { (pfloat: NonZeroFloat) =>
        def widen(value: Double): Double = value
        widen(pfloat) shouldEqual widen(pfloat.toFloat)
      }
      /*forAll { (pfloat: NonZeroFloat) =>
        def widen(value: NonZeroDouble): NonZeroDouble = value
        widen(pfloat) shouldEqual widen(NonZeroDouble.from(pfloat.toFloat).get)
      }*/
    }
  }
  it("should offer an ensuringValid method that takes a Float => Float, throwing AssertionError if the result is invalid") {
    NonZeroFloat(33.0f).ensuringValid(_ + 1.0f) shouldEqual NonZeroFloat(34.0f)
    NonZeroFloat(33.0f).ensuringValid(_ => Float.PositiveInfinity) shouldEqual NonZeroFloat.ensuringValid(Float.PositiveInfinity)
    NonZeroFloat(33.0f).ensuringValid(_ => Float.NegativeInfinity) shouldEqual NonZeroFloat.ensuringValid(Float.NegativeInfinity)
    an [AssertionError] should be thrownBy { NonZeroFloat.MaxValue.ensuringValid(_ => Float.NaN) }
    an [AssertionError] should be thrownBy { NonZeroFloat.MaxValue.ensuringValid(_ - NonZeroFloat.MaxValue) }
  }
}