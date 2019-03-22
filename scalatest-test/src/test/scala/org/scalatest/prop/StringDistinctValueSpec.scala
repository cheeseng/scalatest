/*
 * Copyright 2001-2019 Artima, Inc.
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
package org.scalatest.prop

import org.scalatest._
import org.scalatest.funspec._
import org.scalatest.prop._

class StringDistinctValueSpec extends AnyFunSpec with Matchers with PropertyChecks {

  describe("Distinct when") {

    sealed abstract class Distinct extends Product with Serializable  

    object Distinct {
      def precondition[T](t: T*): Boolean = 
        t.distinct.size == t.size

      final case class Two[T](first: T, second: T) extends Distinct {
        require(precondition(first, second))
      }

      object Two {
        implicit def gen[T: Generator]: Generator[Distinct.Two[T]] = {
          val g = implicitly[Generator[T]]

          for {
            first <- g
            second <- g
            // filters everything out due to immutable Randomizers
            if Distinct.precondition(first, second)
          } yield Distinct.Two(first, second)
        }
      }
    }

    it("adding the same element twice should simply ignore the input") {
      forAll { distinct: Distinct.Two[String] =>
        import distinct._

        Set(first, second, second) shouldBe Set(first, second)
        Set(first, second, second).size shouldBe 2

        Set(first, second, first) shouldBe Set(first, second)
        Set(first, second, first).size shouldBe 2
      }
    }

  }


}