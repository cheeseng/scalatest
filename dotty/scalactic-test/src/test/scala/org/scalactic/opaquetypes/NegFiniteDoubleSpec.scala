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

import NegDoubles.NegFiniteDouble

class NegFiniteDoubleSpec extends AnyFunSpec with Matchers {
  describe("A NegFiniteDouble") {
    it("should offer a from factory method") {
      assert(NegFiniteDouble.from(-50.23).get.value == -50.23)
      assert(NegFiniteDouble.from(0.0) == None)
    }

    it("should offer an ensuringValid factory method") {
      assert(NegFiniteDouble.ensuringValid(-50.23).value == -50.23)
      assertThrows[AssertionError](NegFiniteDouble.ensuringValid(0.0))
    }

    it("should offer a tryingValid factory method") {
      import scala.util.{Success, Failure}
      assert(NegFiniteDouble.tryingValid(-50.0).get.value == -50.0)
      assert(NegFiniteDouble.tryingValid(0.0).failed.get.isInstanceOf[AssertionError])
    }

    it("should offer a passOrElse factory method") {
      import org.scalactic.{Pass, Fail}
      assert(NegFiniteDouble.passOrElse(-50.0)(_ => "fail") == Pass)
      assert(NegFiniteDouble.passOrElse(0.0)(i => f"$i%.1f did not taste good") == Fail("0.0 did not taste good"))
    }

    it("should offer a goodOrElse factory method") {
      import org.scalactic.{Good, Bad}
      assert(NegFiniteDouble.goodOrElse(-50.3)(_ => "fail").get.value == -50.3)
      assert(NegFiniteDouble.goodOrElse(0.0)(i => f"$i%.1f did not taste good") == Bad("0.0 did not taste good"))
    }

    it("should offer a rightOrElse factory method") {
      assert(NegFiniteDouble.rightOrElse(-50.3)(_ => "fail").toOption.get.value == -50.3)
      assert(NegFiniteDouble.rightOrElse(0.0)(i => f"$i%.1f did not taste good") == Left("0.0 did not taste good"))
    }

    it("should offer an isValid predicate method") {
      assert(NegFiniteDouble.isValid(-50.23) == true)
      assert(NegFiniteDouble.isValid(0.0) == false)
    }

    it("should offer a fromOrElse factory method") {
      assert(NegFiniteDouble.fromOrElse(-50.23, NegFiniteDouble.ensuringValid(-42.0)).value == -50.23)
      assert(NegFiniteDouble.fromOrElse(0.0, NegFiniteDouble.ensuringValid(-42.0)).value == -42.0)
    }

    it("should offer MaxValue and MinValue factory methods") {
      assert(NegFiniteDouble.MaxValue.value == NegFiniteDouble.ensuringValid(-Double.MinPositiveValue).value)
      assert(NegFiniteDouble.MinValue.value == NegFiniteDouble.ensuringValid(Double.MinValue).value)
    }

    it("should be sortable") {
      val xs = List(NegFiniteDouble.ensuringValid(-2.2), NegFiniteDouble.ensuringValid(-4.4), NegFiniteDouble.ensuringValid(-1.1), NegFiniteDouble.ensuringValid(-3.3))
      val sorted = xs.sorted
      assert(sorted(0).value == NegFiniteDouble.ensuringValid(-4.4).value)
      assert(sorted(1).value == NegFiniteDouble.ensuringValid(-3.3).value)
      assert(sorted(2).value == NegFiniteDouble.ensuringValid(-2.2).value)
      assert(sorted(3).value == NegFiniteDouble.ensuringValid(-1.1).value)
    }
  }
}
