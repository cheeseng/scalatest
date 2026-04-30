/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalactic.opaquetypes

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import NegDoubles.{NegZDouble, NegZFiniteDouble}

class NegZFiniteDoubleSpec extends AnyFunSpec with Matchers {
  describe("when created with apply method") {
    it("should compile when -8 is passed in") {
      "NegZFiniteDouble.ensuringValid(-8)" should compile
      NegZFiniteDouble.ensuringValid(-8) shouldEqual NegZFiniteDouble(-8.0)
      "NegZFiniteDouble.ensuringValid(-8L)" should compile
      NegZFiniteDouble.ensuringValid(-8L) shouldEqual NegZFiniteDouble(-8.0)
      "NegZFiniteDouble.ensuringValid(-8.0)" should compile
      NegZFiniteDouble.ensuringValid(-8.0) shouldEqual NegZFiniteDouble(-8.0)
    }

    it("should compile when 0 is passed in") {
      "NegZFiniteDouble.ensuringValid(0)" should compile
      NegZFiniteDouble.ensuringValid(0) shouldEqual NegZFiniteDouble(0.0)
      "NegZFiniteDouble.ensuringValid(0L)" should compile
      NegZFiniteDouble.ensuringValid(0L) shouldEqual NegZFiniteDouble(0.0)
      "NegZFiniteDouble.ensuringValid(0.0)" should compile
      NegZFiniteDouble.ensuringValid(0.0) shouldEqual NegZFiniteDouble(0.0)
    }

    it("should throw AssertionError when 8 is passed in") {
      an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(8)
      an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(8L)
      an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(8.0)
    }

    it("should throw AssertionError when x is passed in") {
      val x: Int = 8
      an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(x)
      val b: Long = 8L
      an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(b)
      val c: Double = 8.0
      an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(c)
    }
  }

  describe("when specified as a plain-old Double") {
    def takesNegZFiniteDouble(n: NegZFiniteDouble): Double = n

    it("should compile when -8 is passed in") {
      "takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(-8))" should compile
      takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(-8)) shouldEqual -8.0
      "takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(-8L))" should compile
      takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(-8L)) shouldEqual -8.0
      "takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(-8.0))" should compile
      takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(-8.0)) shouldEqual -8.0
    }

    it("should compile when 0 is passed in") {
      "takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(0))" should compile
      takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(0)) shouldEqual 0.0
      "takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(0L))" should compile
      takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(0L)) shouldEqual 0.0
      "takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(0.0))" should compile
      takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(0.0)) shouldEqual 0.0
    }

    it("should throw AssertionError when 8 is passed in") {
      an [AssertionError] should be thrownBy takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(8))
      an [AssertionError] should be thrownBy takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(8L))
      an [AssertionError] should be thrownBy takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(8.0))
    }

    it("should throw AssertionError when x is passed in") {
      val x: Int = 8
      an [AssertionError] should be thrownBy takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(x))
      val b: Long = 8L
      an [AssertionError] should be thrownBy takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(b))
      val c: Double = 8.0
      an [AssertionError] should be thrownBy takesNegZFiniteDouble(NegZFiniteDouble.ensuringValid(c))
    }
  }

  /*describe("A NegZFiniteDouble") {
    describe("should offer a from factory method that") {
      it("returns Some[NegZFiniteDouble] if the passed Double is less than or equal to 0 and finite") {
        NegZFiniteDouble.from(0.0).get.value shouldBe 0.0
        NegZFiniteDouble.from(-50.23).get.value shouldBe -50.23
        NegZFiniteDouble.from(-100.0).get.value shouldBe -100.0
      }
      it("returns None if the passed Double is greater than 0 or not finite") {
        NegZFiniteDouble.from(0.00001) shouldBe None
        NegZFiniteDouble.from(99.9) shouldBe None
        NegZFiniteDouble.from(Double.PositiveInfinity) shouldBe None
        NegZFiniteDouble.from(Double.NegativeInfinity) shouldBe None
        NegZFiniteDouble.from(Double.NaN) shouldBe None
      }
    }

    describe("should offer an ensuringValid factory method that") {
      it("returns NegZFiniteDouble if the passed Double is less than or equal to 0 and finite") {
        NegZFiniteDouble.ensuringValid(0.0) shouldBe 0.0
        NegZFiniteDouble.ensuringValid(-50.23) shouldBe -50.23
        NegZFiniteDouble.ensuringValid(-100.0) shouldBe -100.0
      }
      it("throws AssertionError if the passed Double is greater than 0 or not finite") {
        an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(0.00001)
        an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(99.9)
        an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(Double.PositiveInfinity)
        an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(Double.NegativeInfinity)
        an [AssertionError] should be thrownBy NegZFiniteDouble.ensuringValid(Double.NaN)
      }
    }

    describe("should offer a tryingValid factory method that") {
      import scala.util.{Success, Failure}
      it("returns a NegZFiniteDouble wrapped in a Success if the passed Double is less than or equal to 0 and finite") {
        NegZFiniteDouble.tryingValid(0.0) shouldBe Success(0.0)
        NegZFiniteDouble.tryingValid(-50.0) shouldBe Success(-50.0)
        NegZFiniteDouble.tryingValid(-100.0) shouldBe Success(-100.0)
      }
      it("returns an AssertionError wrapped in a Failure if the passed Double is greater than 0 or not finite") {
        NegZFiniteDouble.tryingValid(0.00001).failed.get shouldBe a [AssertionError]
        NegZFiniteDouble.tryingValid(99.9).failed.get shouldBe a [AssertionError]
        NegZFiniteDouble.tryingValid(Double.PositiveInfinity).failed.get shouldBe a [AssertionError]
        NegZFiniteDouble.tryingValid(Double.NegativeInfinity).failed.get shouldBe a [AssertionError]
        NegZFiniteDouble.tryingValid(Double.NaN).failed.get shouldBe a [AssertionError]
      }
    }

    describe("should offer a passOrElse factory method that") {
      import org.scalactic.{Pass, Fail}
      it("returns a Pass if the given Double is less than or equal to 0 and finite") {
        NegZFiniteDouble.passOrElse(0.0)(_ => "fail") shouldBe Pass
        NegZFiniteDouble.passOrElse(-50.0)(_ => "fail") shouldBe Pass
        NegZFiniteDouble.passOrElse(-100.0)(_ => "fail") shouldBe Pass
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is greater than 0 or not finite, wrapped in a Fail") {
        NegZFiniteDouble.passOrElse(0.00001)(i => s"$i did not taste good") shouldBe Fail("0.00001 did not taste good")
        NegZFiniteDouble.passOrElse(99.0)(i => i) shouldBe Fail(99.0)
      }
    }

    describe("should offer a goodOrElse factory method that") {
      import org.scalactic.{Good, Bad}
      it("returns a NegZFiniteDouble wrapped in a Good if the given Double is less than or equal to 0 and finite") {
        NegZFiniteDouble.goodOrElse(0.0)(_ => "fail") shouldBe Good(0.0)
        NegZFiniteDouble.goodOrElse(-50.0)(_ => "fail") shouldBe Good(-50.0)
        NegZFiniteDouble.goodOrElse(-100.0)(_ => "fail") shouldBe Good(-100.0)
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is greater than 0 or not finite, wrapped in a Bad") {
        NegZFiniteDouble.goodOrElse(0.00001)(i => s"$i did not taste good") shouldBe Bad("0.00001 did not taste good")
        NegZFiniteDouble.goodOrElse(99.0)(i => i) shouldBe Bad(99.0)
      }
    }

    describe("should offer a rightOrElse factory method that") {
      it("returns a NegZFiniteDouble wrapped in a Right if the given Double is less than or equal to 0 and finite") {
        NegZFiniteDouble.rightOrElse(0.0)(_ => "fail") shouldBe Right(0.0)
        NegZFiniteDouble.rightOrElse(-50.0)(_ => "fail") shouldBe Right(-50.0)
        NegZFiniteDouble.rightOrElse(-100.0)(_ => "fail") shouldBe Right(-100.0)
      }
      it("returns an error value produced by passing the given Double to the given function if the passed Double is greater than 0 or not finite, wrapped in a Left") {
        NegZFiniteDouble.rightOrElse(0.00001)(i => s"$i did not taste good") shouldBe Left("0.00001 did not taste good")
        NegZFiniteDouble.rightOrElse(99.0)(i => i) shouldBe Left(99.0)
      }
    }

    describe("should offer an isValid predicate method that") {
      it("returns true if the passed Double is less than or equal to 0 and finite") {
        NegZFiniteDouble.isValid(0.0) shouldBe true
        NegZFiniteDouble.isValid(-50.23) shouldBe true
        NegZFiniteDouble.isValid(-100.0) shouldBe true
        NegZFiniteDouble.isValid(0.00001) shouldBe false
        NegZFiniteDouble.isValid(99.9) shouldBe false
      }
    }

    describe("should offer a fromOrElse factory method that") {
      it("returns a NegZFiniteDouble if the passed Double is less than or equal to 0 and finite") {
        NegZFiniteDouble.fromOrElse(0.0, NegZFiniteDouble.ensuringValid(-42.0)) shouldBe 0.0
        NegZFiniteDouble.fromOrElse(-50.23, NegZFiniteDouble.ensuringValid(-42.0)) shouldBe -50.23
        NegZFiniteDouble.fromOrElse(-100.0, NegZFiniteDouble.ensuringValid(-42.0)) shouldBe -100.0
      }
      it("returns a given default if the passed Double is greater than 0 or not finite") {
        NegZFiniteDouble.fromOrElse(0.00001, NegZFiniteDouble.ensuringValid(-42.0)) shouldBe -42.0
        NegZFiniteDouble.fromOrElse(99.9, NegZFiniteDouble.ensuringValid(-42.0)) shouldBe -42.0
      }
    }

    it("should offer MaxValue and MinValue factory methods") {
      NegZFiniteDouble.MaxValue shouldEqual NegZFiniteDouble.ensuringValid(0.0)
      NegZFiniteDouble.MinValue shouldEqual NegZFiniteDouble.ensuringValid(Double.MinValue)
    }

    it("should be sortable") {
      val xs = List(NegZFiniteDouble.ensuringValid(-2.2), NegZFiniteDouble.ensuringValid(0.0), NegZFiniteDouble.ensuringValid(-1.1), NegZFiniteDouble.ensuringValid(-3.3))
      xs.sorted shouldEqual List(NegZFiniteDouble.ensuringValid(-3.3), NegZFiniteDouble.ensuringValid(-2.2), NegZFiniteDouble.ensuringValid(-1.1), NegZFiniteDouble.ensuringValid(0.0))
    }

    it("should be a subtype of NegZDouble") {
      val negZFiniteDouble: NegZFiniteDouble = NegZFiniteDouble.ensuringValid(-1.0)
      val negZDouble: NegZDouble = negZFiniteDouble
      negZDouble shouldEqual NegZDouble.ensuringValid(-1.0)
    }
  }*/
}
