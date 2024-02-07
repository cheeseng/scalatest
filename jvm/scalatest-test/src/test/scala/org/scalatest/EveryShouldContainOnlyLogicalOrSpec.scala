/*
* Copyright 2001-2013 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.scalatest

import org.scalactic.{Equality, Every, One, Many, Prettifier}
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class EveryShouldContainOnlyLogicalOrSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }

  val invertedListOfStringEquality =
    new Equality[Every[String]] {
      def areEqual(a: Every[String], b: Any): Boolean = a != b
    }

  private def upperCase(value: Any): Any =
    value match {
      case l: Every[_] => l.map(upperCase(_))
      case s: String => s.toUpperCase
      case c: Char => c.toString.toUpperCase.charAt(0)
      case (s1: String, s2: String) => (s1.toUpperCase, s2.toUpperCase)
      case e: java.util.Map.Entry[_, _] =>
        (e.getKey, e.getValue) match {
          case (k: String, v: String) => Entry(k.toUpperCase, v.toUpperCase)
          case _ => value
        }
      case _ => value
    }

  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = upperCase(a) == upperCase(b)
    }

  //ADDITIONAL//

  val fileName: String = "EveryShouldContainOnlyLogicalOrSpec.scala"

  describe("an Every") {

    val fumList: Every[String] = Every("fum", "foe", "fie", "fee")
    val toList: Every[String] = Every("you", "to", "birthday", "happy")

    describe("when used with (contain only (..) or contain only (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain only ("fee", "fie", "foe", "fum") or contain only ("fie", "fee", "fum", "foe"))
        fumList should (contain only ("fee", "fie", "foe", "fam") or contain only ("fie", "fee", "fum", "foe"))
        fumList should (contain only ("fee", "fie", "foe", "fum") or contain only ("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain only ("fee", "fie", "foe", "fam") or contain only ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fam\"") + ", and " + Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain only ("FEE", "FIE", "FOE", "FUM") or contain only ("FIE", "FEE", "FUM", "FOE"))
        fumList should (contain only ("FEE", "FIE", "FOE", "FAM") or contain only ("FIE", "FEE", "FUM", "FOE"))
        fumList should (contain only ("FEE", "FIE", "FOE", "FUM") or contain only ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain only ("FEE", "FIE", "FOE", "FAM") or (contain only ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", and " + Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain only ("FEE", "FIE", "FOE", "FUM") or contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain only ("FEE", "FIE", "FOE", "FAM") or contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain only ("FEE", "FIE", "FOE", "FUM") or contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain only ("FEE", "FIE", "FOE", "FAM") or contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", and " + Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (contain only (" FEE ", " FIE ", " FOE ", " FUM ") or contain only (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        if (ScalaTestVersions.BuiltForScalaVersion != "2.13" && !ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) { // For 2.13 and 3.x, the compiler will pass in args with single argument ().
          val e1 = intercept[exceptions.NotAllowedException] {
            fumList should (contain only() or contain only("fie", "fee", "fum", "foe"))
          }
          e1.failedCodeFileName.get should be(fileName)
          e1.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e1.message should be(Some(Resources.onlyEmpty))

          val e2 = intercept[exceptions.NotAllowedException] {
            fumList should (contain only("fie", "fee", "fum", "foe") or contain only())
          }
          e2.failedCodeFileName.get should be(fileName)
          e2.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e2.message should be(Some(Resources.onlyEmpty))
        } else {
          val e1 = intercept[exceptions.NotAllowedException] {
            fumList should (contain.only() or contain only("fie", "fee", "fum", "foe"))
          }
          e1.failedCodeFileName.get should be(fileName)
          e1.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e1.message should be(Some(Resources.onlyEmpty))

          val e2 = intercept[exceptions.NotAllowedException] {
            fumList should (contain only("fie", "fee", "fum", "foe") or contain.only())
          }
          e2.failedCodeFileName.get should be(fileName)
          e2.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e2.message should be(Some(Resources.onlyEmpty))
        }
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only ("fee", "fie", "foe", "fie", "fum") or contain only ("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only ("fie", "fee", "fum", "foe") or contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single Iterable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          fumList should (contain only Many("fee", "fie", "foe", "fam") or contain only Many("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnlyWithFriendlyReminder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, Many("fee", "fie", "foe", "fam"))) + ", and " + Resources.didNotContainOnlyWithFriendlyReminder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, Many("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
      }
    }

    describe("when used with (equal (..) and contain only (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) or contain only ("fie", "fee", "fum", "foe"))
        fumList should (equal (toList) or contain only ("fie", "fee", "fum", "foe"))
        fumList should (equal (fumList) or contain only ("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain only ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", and " + Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain only ("FIE", "FEE", "FUM", "FOE"))
        fumList should (equal (toList) or contain only ("FIE", "FEE", "FUM", "FOE"))
        fumList should (equal (fumList) or contain only ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain only ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", and " + Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (equal (toList) or contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", and " + Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain only Set(" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        if (ScalaTestVersions.BuiltForScalaVersion != "2.13" && !ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) { // For 2.13 and 3.x, the compiler will pass in args with single argument ().
          val e1 = intercept[exceptions.NotAllowedException] {
            fumList should (equal(fumList) or contain only())
          }
          e1.failedCodeFileName.get should be(fileName)
          e1.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e1.message should be(Some(Resources.onlyEmpty))
        } else {
          val e1 = intercept[exceptions.NotAllowedException] {
            fumList should (equal(fumList) or contain.only())
          }
          e1.failedCodeFileName.get should be(fileName)
          e1.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e1.message should be(Some(Resources.onlyEmpty))
        }
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) or contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single Iterable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain only Many("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", and " + Resources.didNotContainOnlyWithFriendlyReminder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, Many("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
      }
    }

    describe("when used with (be (..) and contain only (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be (fumList) or contain only ("fie", "fee", "fum", "foe"))
        fumList should (be (toList) or contain only ("fie", "fee", "fum", "foe"))
        fumList should (be (fumList) or contain only ("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or contain only ("fie", "fee", "fam", "foe"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", and " + Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"fie\", \"fee\", \"fam\", \"foe\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) or contain only ("FIE", "FEE", "FUM", "FOE"))
        fumList should (be (toList) or contain only ("FIE", "FEE", "FUM", "FOE"))
        fumList should (be (fumList) or contain only ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or (contain only ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", and " + Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (be (fumList) or contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (be (toList) or contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (be (fumList) or contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (toList) or contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", and " + Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) or contain only (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        if (ScalaTestVersions.BuiltForScalaVersion != "2.13" && !ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) { // For 2.13 and 3.x, the compiler will pass in args with single argument ().
          val e1 = intercept[exceptions.NotAllowedException] {
            fumList should (be(fumList) or contain only())
          }
          e1.failedCodeFileName.get should be(fileName)
          e1.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e1.message should be(Some(Resources.onlyEmpty))
        } else {
          val e1 = intercept[exceptions.NotAllowedException] {
            fumList should (be(fumList) or contain.only())
          }
          e1.failedCodeFileName.get should be(fileName)
          e1.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e1.message should be(Some(Resources.onlyEmpty))
        }
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be (fumList) or contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single Iterable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or contain only Many("fie", "fee", "fam", "foe"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", and " + Resources.didNotContainOnlyWithFriendlyReminder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, Many("fie", "fee", "fam", "foe"))), fileName, thisLineNumber - 2)
      }
    }

    describe("when used with (contain only (..) and be (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain only ("fie", "fee", "fum", "foe") or be (fumList))
        fumList should (contain only ("fie", "fee", "fam", "foe") or be (fumList))
        fumList should (contain only ("fie", "fee", "fum", "foe") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain only ("fee", "fie", "foe", "fam") or be (toList))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fam\"") + ", and " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain only ("FIE", "FEE", "FUM", "FOE") or be (fumList))
        fumList should (contain only ("FIE", "FEE", "FAM", "FOE") or be (fumList))
        fumList should (contain only ("FIE", "FEE", "FUM", "FOE") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain only ("FEE", "FIE", "FOE", "FAM") or be (toList))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", and " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (contain only ("FIE", "FEE", "FUM", "FOE") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain only ("FIE", "FEE", "FAM", "FOE") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain only ("FIE", "FEE", "FUM", "FOE") or be (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain only ("FEE", "FIE", "FOE", "FAM") or be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnly(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", and " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        (fumList should (contain only (" FEE ", " FIE ", " FOE ", " FUM ") or be (fumList))) (after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        if (ScalaTestVersions.BuiltForScalaVersion != "2.13" && !ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) { // For 2.13 and 3.x, the compiler will pass in args with single argument ().
          val e1 = intercept[exceptions.NotAllowedException] {
            fumList should (contain only() or be(fumList))
          }
          e1.failedCodeFileName.get should be(fileName)
          e1.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e1.message should be(Some(Resources.onlyEmpty))
        } else {
          val e1 = intercept[exceptions.NotAllowedException] {
            fumList should (contain.only() or be(fumList))
          }
          e1.failedCodeFileName.get should be(fileName)
          e1.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e1.message should be(Some(Resources.onlyEmpty))
        }
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only ("fee", "fie", "foe", "fie", "fum") or be (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single Iterable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          fumList should (contain only Many("fee", "fie", "foe", "fam") or be (toList))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnlyWithFriendlyReminder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, Many("fee", "fie", "foe", "fam"))) + ", and " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
      }
    }

    describe("when used with (not contain only (..) and not contain only (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain only ("fee", "fie", "foe", "fuu") or not contain only ("fie", "fee", "fuu", "foe"))
        fumList should (not contain only ("fee", "fie", "foe", "fum") or not contain only ("fie", "fee", "fuu", "foe"))
        fumList should (not contain only ("fee", "fie", "foe", "fuu") or not contain only ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain only ("fee", "fie", "foe", "fum") or not contain only ("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources.containedOnly(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources.containedOnly(decorateToStringValue(prettifier, fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain only ("FEE", "FIE", "FOE", "FUU") or not contain only ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not contain only ("FEE", "FIE", "FOE", "FUM") or not contain only ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not contain only ("FEE", "FIE", "FOE", "FUU") or not contain only ("FIE", "FEE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain only ("FEE", "FIE", "FOE", "FUM") or not contain only ("FIE", "FEE", "FUM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources.containedOnly(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources.containedOnly(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not contain only ("FEE", "FIE", "FOE", "FUU") or not contain only ("FIE", "FEE", "FUU", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain only ("FEE", "FIE", "FOE", "FUM") or not contain only ("FIE", "FEE", "FUU", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain only ("FEE", "FIE", "FOE", "FUU") or not contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain only ("FEE", "FIE", "FOE", "FUM") or not contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.containedOnly(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources.containedOnly(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (contain only (" FEE ", " FIE ", " FOE ", " FUM ") or contain only (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain only () or not contain only ("fie", "fee", "fuu", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain only ("fie", "fee", "fuu", "foe") or not contain only ())
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyEmpty))
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain only ("fee", "fie", "foe", "fie", "fum") or not contain only ("fie", "fee", "fuu", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain only ("fie", "fee", "fuu", "foe") or not contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single Iterable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          One(Many("fee", "fie", "foe", "fum")) should (not contain only (Many("fee", "fie", "foe", "fum")) or not contain only (Many("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources.containedOnlyWithFriendlyReminder(decorateToStringValue(prettifier, One(Many("fee", "fie", "foe", "fum"))), decorateToStringValue(prettifier, Many("fee", "fie", "foe", "fum"))) + ", and " + Resources.containedOnlyWithFriendlyReminder(decorateToStringValue(prettifier, One(Many("fee", "fie", "foe", "fum"))), decorateToStringValue(prettifier, Many("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }
    }

    describe("when used with (not equal (..) and not contain only (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) or not contain only ("fie", "fee", "fuu", "foe"))
        fumList should (not equal (fumList) or not contain only ("fie", "fee", "fuu", "foe"))
        fumList should (not equal (toList) or not contain only ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not contain only ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", and " + Resources.containedOnly(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain only ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not equal (fumList) or not contain only ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not equal (toList) or not contain only ("FIE", "FEE", "FUM", "FOE"))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain only ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", and " + Resources.containedOnly(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not equal (fumList) or not contain only ("FIE", "FEE", "FUU", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain only ("FIE", "FEE", "FUU", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", and " + Resources.containedOnly(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (not contain only (" FEE ", " FIE ", " FOE ", " FUU ") or not contain only (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) or not contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) or not contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single Iterable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          One(Many("fee", "fie", "foe", "fum")) should (not equal (One(Many("fee", "fie", "foe", "fum"))) or not contain only (Many("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, One(Many("fee", "fie", "foe", "fum"))), decorateToStringValue(prettifier, One(Many("fee", "fie", "foe", "fum")))) + ", and " + Resources.containedOnlyWithFriendlyReminder(decorateToStringValue(prettifier, One(Many("fee", "fie", "foe", "fum"))), decorateToStringValue(prettifier, Many("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }
    }

    describe("when used with (not be (..) and not contain only (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) or not contain only ("fie", "fee", "fuu", "foe"))
        fumList should (not be (fumList) or not contain only ("fie", "fee", "fuu", "foe"))
        fumList should (not be (toList) or not contain only ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or not contain only ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", and " + Resources.containedOnly(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) or not contain only ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not be (fumList) or not contain only ("FIE", "FEE", "FUU", "FOE"))
        fumList should (not be (toList) or not contain only ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not contain only ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", and " + Resources.containedOnly(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (fumList should (not be (toList) or not contain only ("FIE", "FEE", "FUU", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (not be (fumList) or not contain only ("FIE", "FEE", "FUU", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (not be (toList) or not contain only ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not contain only ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", and " + Resources.containedOnly(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (not contain only (" FEE ", " FIE ", " FOE ", " FUU ") or not contain only (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be (toList) or not contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be (toList) or not contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single Iterable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          One(Many("fee", "fie", "foe", "fum")) should (not be (One(Many("fee", "fie", "foe", "fum"))) or not contain only (Many("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, One(Many("fee", "fie", "foe", "fum"))), decorateToStringValue(prettifier, One(Many("fee", "fie", "foe", "fum")))) + ", and " + Resources.containedOnlyWithFriendlyReminder(decorateToStringValue(prettifier, One(Many("fee", "fie", "foe", "fum"))), decorateToStringValue(prettifier, Many("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }
    }

  }

  describe("collection of Lists") {

    val list1s: Every[Every[Int]] = Every(Every(3, 2, 1), Every(3, 2, 1), Every(3, 2, 1))
    val lists: Every[Every[Int]] = Every(Every(3, 2, 1), Every(3, 2, 1), Every(4, 3, 2))
    val hiLists: Every[Every[String]] = Every(Every("hi", "hello"), Every("hi", "hello"), Every("hi", "hello"))
    val toLists: Every[Every[String]] = Every(Every("you", "to"), Every("you", "to"), Every("you", "to"))

    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String =
      "'all' inspection failed, because: \n" +
        "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
        "in " + decorateToStringValue(prettifier, left)

    describe("when used with (contain only (..) and contain only (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain only (3, 2, 1) or contain only (1, 3, 2))
        all (list1s) should (contain only (3, 2, 5) or contain only (1, 3, 2))
        all (list1s) should (contain only (3, 2, 1) or contain only (2, 3, 4))

        atLeast (2, lists) should (contain only (3, 1, 2) or contain only (1, 2, 3))
        atLeast (2, lists) should (contain only (3, 6, 5) or contain only (1, 3, 2))
        atLeast (2, lists) should (contain only (3, 1, 2) or contain only (8, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (lists) should (contain only (3, 1, 2) or contain only (1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, Many(4, 3, 2)) + " did not contain only " + "(3, 1, 2)" + ", and " + decorateToStringValue(prettifier, Many(4, 3, 2)) + " did not contain only " + "(1, 3, 2)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (contain only ("HELLO", "HI") or contain only ("hello", "hi"))
        all (hiLists) should (contain only ("HELLO", "HO") or contain only ("hello", "hi"))
        all (hiLists) should (contain only ("HELLO", "HI") or contain only ("hello", "ho"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain only ("HELLO", "HO") or contain only ("hello", "ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HO\")" + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"hello\", \"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain only ("HELLO", "HI") or contain only ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain only ("HELLO", "HO") or contain only ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain only ("HELLO", "HI") or contain only ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain only ("HELLO", "HO") or contain only ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HO\")" + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"hello\", \"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        if (ScalaTestVersions.BuiltForScalaVersion != "2.13" && !ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) { // For 2.13 and 3.x, the compiler will pass in args with single argument ().
          val e1 = intercept[exceptions.NotAllowedException] {
            all(list1s) should (contain only() or contain only(1, 3, 2))
          }
          e1.failedCodeFileName.get should be(fileName)
          e1.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e1.message should be(Some(Resources.onlyEmpty))

          val e2 = intercept[exceptions.NotAllowedException] {
            all(list1s) should (contain only(1, 3, 2) or contain only())
          }
          e2.failedCodeFileName.get should be(fileName)
          e2.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e2.message should be(Some(Resources.onlyEmpty))
        } else {
          val e1 = intercept[exceptions.NotAllowedException] {
            all(list1s) should (contain.only() or contain only(1, 3, 2))
          }
          e1.failedCodeFileName.get should be(fileName)
          e1.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e1.message should be(Some(Resources.onlyEmpty))

          val e2 = intercept[exceptions.NotAllowedException] {
            all(list1s) should (contain only(1, 3, 2) or contain.only())
          }
          e2.failedCodeFileName.get should be(fileName)
          e2.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e2.message should be(Some(Resources.onlyEmpty))
        }
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain only (3, 2, 2, 1) or contain only (1, 3, 2))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain only (1, 3, 2) or contain only (3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single Iterable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          all (One(One(Many("hi", "hello")))) should (contain only Many("HELLO", "HO") or contain only Many("hello", "ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, One(Many("hi", "hello"))) + " did not contain only " + "(" + decorateToStringValue(prettifier, Many("HELLO", "HO")) + "), did you forget to say : _*" + ", and " + decorateToStringValue(prettifier, One(Many("hi", "hello"))) + " did not contain only " + "(" + decorateToStringValue(prettifier, Many("hello", "ho")) + "), did you forget to say : _*", thisLineNumber - 2, One(One(Many("hi", "hello")))), fileName, thisLineNumber - 2)
      }
    }

    describe("when used with (be (..) and contain only (..))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be (Many(3, 2, 1)) or contain only (1, 2, 3))
        all (list1s) should (be (Many(2, 3, 4)) or contain only (1, 2, 3))
        all (list1s) should (be (Many(3, 2, 1)) or contain only (2, 3, 4))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (be (Many(2, 3, 4)) or contain only (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many(3, 2, 1)) + " was not equal to " + decorateToStringValue(prettifier, Many(2, 3, 4)) + ", and " + decorateToStringValue(prettifier, Many(3, 2, 1)) + " did not contain only " + "(2, 3, 4)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (be (Many("hi", "hello")) or contain only ("HELLO", "HI"))
        all (hiLists) should (be (Many("ho", "hello")) or contain only ("HELLO", "HI"))
        all (hiLists) should (be (Many("hi", "hello")) or contain only ("HELLO", "HO"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (Many("ho", "hello")) or contain only ("HELLO", "HO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, Many("ho", "hello")) + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be (Many("hi", "hello")) or contain only ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (Many("ho", "hello")) or contain only ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (Many("hi", "hello")) or contain only ("HELLO", "HO"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (Many("ho", "hello")) or contain only ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, Many("ho", "hello")) + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " did not contain only " + "(\"HELLO\", \"HO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        if (ScalaTestVersions.BuiltForScalaVersion != "2.13" && !ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) { // For 2.13 and 3.x, the compiler will pass in args with single argument ().
          val e1 = intercept[exceptions.NotAllowedException] {
            all(list1s) should (be(Many(3, 2, 1)) or contain only())
          }
          e1.failedCodeFileName.get should be(fileName)
          e1.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e1.message should be(Some(Resources.onlyEmpty))
        } else {
          val e1 = intercept[exceptions.NotAllowedException] {
            all(list1s) should (be(Many(3, 2, 1)) or contain.only())
          }
          e1.failedCodeFileName.get should be(fileName)
          e1.failedCodeLineNumber.get should be(thisLineNumber - 3)
          e1.message should be(Some(Resources.onlyEmpty))
        }
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be (Many(3, 2, 1)) or contain only (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single Iterable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          all (One(One(Many(3, 2, 1)))) should (be (Many(2, 3, 4)) or contain only Many(2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, One(Many(3, 2, 1))) + " was not equal to " + decorateToStringValue(prettifier, Many(2, 3, 4)) + ", and " + decorateToStringValue(prettifier, One(Many(3, 2, 1))) + " did not contain only (" + decorateToStringValue(prettifier, Many(2, 3, 4)) + "), did you forget to say : _*", thisLineNumber - 2, One(One(Many(3, 2, 1)))), fileName, thisLineNumber - 2)
      }
    }

    describe("when used with (not contain only xx and not contain only xx)") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain only (3, 2, 8) or not contain only (8, 3, 4))
        all (list1s) should (not contain only (1, 2, 3) or not contain only (8, 3, 4))
        all (list1s) should (not contain only (3, 2, 8) or not contain only (2, 3, 1))

        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain only (4, 2, 3) or not contain only (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, Many(4, 3, 2)) + " contained only " + "(4, 2, 3)" + ", and " + decorateToStringValue(prettifier, Many(4, 3, 2)) + " contained only " + "(2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not contain only ("HELLO", "HO") or not contain only ("hello", "ho"))
        all (hiLists) should (not contain only ("HELLO", "HI") or not contain only ("hello", "ho"))
        all (hiLists) should (not contain only ("HELLO", "HO") or not contain only ("hello", "hi"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain only ("HELLO", "HI") or not contain only ("hello", "hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"HELLO\", \"HI\")" + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"hello\", \"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain only ("HELLO", "HO") or not contain only ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain only ("HELLO", "HI") or not contain only ("hello", "ho"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain only ("HELLO", "HO") or not contain only ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain only ("HELLO", "HI") or not contain only ("hello", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"HELLO\", \"HI\")" + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"hello\", \"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain only () or not contain only (8, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain only (8, 3, 4) or not contain only ())
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyEmpty))
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain only (1, 2, 2, 3) or not contain only (8, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain only (8, 3, 4) or not contain only (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single Iterable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          all (One(One(Many(3, 2, 1)))) should (not contain only (Many(3, 2, 1)) or not contain only (Many(3, 2, 1)))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, One(Many(3, 2, 1))) + " contained only (" + decorateToStringValue(prettifier, Many(3, 2, 1)) + "), did you forget to say : _*" + ", and " + decorateToStringValue(prettifier, One(Many(3, 2, 1))) + " contained only (" + decorateToStringValue(prettifier, Many(3, 2, 1)) + "), did you forget to say : _*", thisLineNumber - 2, One(One(Many(3, 2, 1)))), fileName, thisLineNumber - 2)
      }
    }

    describe("when used with (not be (...) and not contain only (...))") {

      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be (One(2)) or not contain only (8, 3, 4))
        all (list1s) should (not be (Many(3, 2, 1)) or not contain only (8, 3, 4))
        all (list1s) should (not be (One(2)) or not contain only (1, 2, 3))

        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be (Many(3, 2, 1)) or not contain only (2, 3, 1))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many(3, 2, 1)) + " was equal to " + decorateToStringValue(prettifier, Many(3, 2, 1)) + ", and " + decorateToStringValue(prettifier, Many(3, 2, 1)) + " contained only " + "(2, 3, 1)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality

        all (hiLists) should (not be (Many("hello", "ho")) or not contain only ("HELLO", "HO"))
        all (hiLists) should (not be (Many("hello", "hi")) or not contain only ("HELLO", "HO"))
        all (hiLists) should (not be (Many("hello", "ho")) or not contain only ("HELLO", "HI"))

        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (Many("hi", "hello")) or not contain only ("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("hi", "hello")) + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"HELLO\", \"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be (Many("hello", "ho")) or not contain only ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (Many("hello", "hi")) or not contain only ("HELLO", "HO"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (Many("hello", "ho")) or not contain only ("HELLO", "HI"))) (decided by upperCaseStringEquality)

        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (Many("hi", "hello")) or not contain only ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, Many("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, Many("hi", "hello")) + ", and " + decorateToStringValue(prettifier, Many("hi", "hello")) + " contained only " + "(\"HELLO\", \"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be (One(2)) or not contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be (One(2)) or not contain only (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))

        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain only (8, 3, 4) or not contain only (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single Iterable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          all (One(One(Many(3, 2, 1)))) should (not be (One(Many(3, 2, 1))) or not contain only (Many(3, 2, 1)))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, One(Many(3, 2, 1))) + " was equal to " + decorateToStringValue(prettifier, One(Many(3, 2, 1))) + ", and " + decorateToStringValue(prettifier, One(Many(3, 2, 1))) + " contained only (" + decorateToStringValue(prettifier, Many(3, 2, 1)) + "), did you forget to say : _*", thisLineNumber - 2, One(One(Many(3, 2, 1)))), fileName, thisLineNumber - 2)
      }
    }
  }
}
