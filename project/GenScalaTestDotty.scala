/*
* Copyright 2001-2015 Artima, Inc.
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

import sbt.IO

import scala.io.Source
import java.io.{File, FileWriter, BufferedWriter}

object GenScalaTestDotty {

  private def uncommentJsExport(line: String): String =
    if (line.trim.startsWith("//DOTTY-ONLY "))
      line.substring(line.indexOf("//DOTTY-ONLY ") + 13)
    else if (line.trim.startsWith("//DOTTY-ONLY "))
      line.substring(line.indexOf("//DOTTY-ONLY ") + 13)
    else
      line

  private def transformLine(line: String): String =
    uncommentJsExport(line)

  private def copyFile(sourceFile: File, destFile: File): File = {
    val destWriter = new BufferedWriter(new FileWriter(destFile))
    try {
      val lines = Source.fromFile(sourceFile).getLines.toList
      var skipMode = false
      for (line <- lines) {
        if (line.trim == "// SKIP-DOTTY-START" || line.trim == "// SKIP-DOTTY-START")
          skipMode = true
        else if (line.trim == "// SKIP-DOTTY-END" || line.trim == "// SKIP-DOTTY-END")
          skipMode = false
        else if (!skipMode) {
          destWriter.write(transformLine(line))
          destWriter.newLine()
        }
      }
      destFile
    }
    finally {
      destWriter.flush()
      destWriter.close()
      println("Copied " + destFile.getAbsolutePath)
    }
  }

  def copyFiles(sourceDirName: String, packageDirName: String, targetDir: File, files: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    files.map { sourceFileName =>
      val sourceFile = new File(sourceDir, sourceFileName)
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified)
        copyFile(sourceFile, destFile)

      destFile
    }
  }

  def copyStartsWithFiles(sourceDirName: String, packageDirName: String, startsWith: String, targetDir: File): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.filter(f => f.isFile && f.getName.startsWith(startsWith) && f.getName.endsWith(".scala")).map { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified)
        copyFile(sourceFile, destFile)

      destFile
    }
  }

  def copyDir(sourceDirName: String, packageDirName: String, targetDir: File, skipList: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.filter(f => f.isFile && !skipList.contains(f.getName) && (f.getName.endsWith(".scala") || f.getName.endsWith(".java"))).map { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified)
        copyFile(sourceFile, destFile)

      destFile
    }
  }

  def copyResourceDir(sourceDirName: String, packageDirName: String, targetDir: File, skipList: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.filter(f => f.isFile && !skipList.contains(f.getName)).map { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified)
        IO.copyFile(sourceFile, destFile)
      destFile
    }
  }

  def genJava(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyFiles("jvm/core/src/main/java/org/scalatest", "org/scalatest", targetDir,
      List(
        "Finders.java",
        "TagAnnotation.java",
        "WrapWith.java",
        "DoNotDiscover.java",
        "Ignore.java"
      )) ++ 
    copyDir("jvm/core/src/main/java/org/scalatest/tags", "org/scalatest/tags", targetDir, List.empty)  
  }

  def genHtml(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyResourceDir("jvm/core/src/main/html", "html", targetDir, List.empty) ++
    copyResourceDir("jvm/core/src/main/resources/images", "images", targetDir, List.empty) ++
    copyResourceDir("jvm/core/src/main/resources/org/scalatest", "org/scalatest", targetDir, List.empty)
  }

  val genScalaPackages: Map[String, List[String]] = 
    Map(
      "org/scalatest" -> List(
        "Assertions.scala",                 // Re-implemented
        "AssertionsMacro.scala",            // Re-implemented
        "CompileMacro.scala",               // Re-implemented
        "DiagrammedAssertions.scala",       // Re-implemented
        "DiagrammedAssertionsMacro.scala",  // Re-implemented
        "DiagrammedExprMacro.scala",        // Re-implemented
        "DiagrammedExpr.scala",             // Re-implemented
        "Expectations.scala",               // Re-implemented
        "ExpectationsMacro.scala",          // Re-implemented
        "StreamlinedXml.scala",             // Hmm, not sure what to do with XML support, let's ask.
        "StreamlinedXmlEquality.scala",     // Hmm, not sure what to do with XML support, let's ask.
        "StreamlinedXmlNormMethods.scala"   // Hmm, not sure what to do with XML support, let's ask.
      ), 
      "org/scalatest/concurrent" -> List.empty, 
      "org/scalatest/diagrams" -> List(
        "Diagrams.scala", 
        "DiagramsMacro.scala"
      ), 
      "org/scalatest/exceptions" -> List.empty, 
      "org/scalatest/enablers" -> List.empty, 
      "org/scalatest/events" -> List.empty, 
      "org/scalatest/fixture" -> List.empty, 
      "org/scalatest/featurespec" -> List.empty, 
      "org/scalatest/funspec" -> List.empty, 
      "org/scalatest/funsuite" -> List.empty, 
      "org/scalatest/freespec" -> List.empty, 
      "org/scalatest/flatspec" -> List.empty, 
      "org/scalatest/matchers" -> List(
        "Matcher.scala",           // Re-implemented with new macro
        "MatchPatternMacro.scala", // Re-implemented with new macro
        "TypeMatcherMacro.scala"   // Re-implemented with new macro
      ), 
      "org/scalatest/matchers/dsl" -> List(
        "BeWord.scala", 
        "JavaCollectionWrapper.scala",
        "JavaMapWrapper.scala",
        "MatchPatternWord.scala",
        "NotWord.scala",
        "ResultOfNotWordForAny.scala"
      ),
      "org/scalatest/expectations" -> List.empty,  
      "org/scalatest/matchers/should" -> List.empty, 
      "org/scalatest/path" -> List.empty, 
      "org/scalatest/prop" -> List.empty, 
      "org/scalatest/propspec" -> List.empty, 
      "org/scalatest/tagobjects" -> List.empty, 
      "org/scalatest/time" -> List.empty, 
      "org/scalatest/verbs" -> List.empty, 
      "org/scalatest/tools" -> List.empty, 
      "org/scalatest/refspec" -> List.empty, 
      "org/scalatest/words" -> List.empty, 
      "org/scalatest/wordspec" -> List.empty
    )

  def genScala(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
    genScalaPackages.flatMap { case (packagePath, skipList) =>
      copyDir("scalatest/src/main/scala/" + packagePath, packagePath, targetDir, skipList)
    }.toList

  def genTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyFiles("jvm/scalatest-test/src/test/scala/org/scalatest", "org/scalatest", targetDir,
      List(
        "AlerterSpec.scala", 
        "AllElementsOfContainMatcherDeciderSpec.scala", 
        "AllElementsOfContainMatcherEqualitySpec.scala", 
        "AllElementsOfContainMatcherSpec.scala", 
        "AllOfContainMatcherDeciderSpec.scala", 
        "AllOfContainMatcherEqualitySpec.scala", 
        "AllOfContainMatcherSpec.scala", 
        //"AllSuiteProp.scala", // skipped because does not compile yet
        //"AMatcherSpec.scala", // skipped because does not compile yet 
        //"AnMatcherSpec.scala",  // skipped because does not compile yet
        //"AnyValMatchersSpec.scala",  // skipped because does not compile yet
        //"AppendedCluesSpec.scala", // skipped because does not compile yet 
        //"ArgsSpec.scala",  // skipped because does not compile yet
        "AsyncEngineSpec.scala", 
        "AsyncFixturesSpec.scala", 
        "AsyncInspectorsSpec.scala", 
        "AsyncPropSpecLikeSpec.scala", 
        "AsyncPropSpecLikeSpec2.scala", 
        "AsyncPropSpecSpec.scala", 
        "AsyncPropSpecSpec2.scala", 
        //"AsyncWordSpecLikeSpec.scala", // skipped because does not compile yet 
        "AsyncWordSpecLikeSpec2.scala", 
        //"AsyncWordSpecSpec.scala", // skipped because does not compile yet 
        "AsyncWordSpecSpec2.scala", 
        //"BeforeAndAfterAllConfigMapSpec.scala", // skipped because does not compile yet 
        //"BeforeAndAfterAllProp.scala", // skipped because does not compile yet 
        //"BeforeAndAfterAllSpec.scala", // skipped because does not compile yet 
        "BeforeAndAfterAsyncSuite.scala", 
        "BeforeAndAfterConfigSuite.scala", 
        "BeforeAndAfterEachAllSuite.scala", 
        "BeforeAndAfterEachAsyncSuite.scala", 
        "BeforeAndAfterEachSuite.scala", 
        "BeforeAndAfterEachTestDataAsyncSuite.scala", 
        "BeforeAndAfterEachTestDataSuite.scala", 
        "BeforeAndAfterSuite.scala", 
        "BigSuite.scala", 
        "BigSuiteSuite.scala", 
        "CancelAfterFailureSpec.scala", 
        //"CatchReporterProp.scala", // skipped because does not compile yet  
        "CatchReporterSpec.scala", 
        //"CheckpointsSpec.scala", // skipped because does not compile yet 
        //"ClassTaggingProp.scala", // skipped because does not compile yet  
        //"ClueSpec.scala", // skipped because does not compile yet  
        "CompleteLastlySpec.scala", 
        "ConfigMapSpec.scala", 
        //"ConfigMapWrapperSuiteSpec.scala", // skipped because does not compile yet  
        "ContainMatcherAndOrDeciderSpec.scala", 
        "ContainMatcherAndOrEqualitySpec.scala", 
        "ContainMatcherAndOrExplicitEqualitySpec.scala", 
        "ContainMatcherAndOrSpec.scala", 
        "CustomMatcherSpec.scala", 
        "DeprecatedAsyncFeatureSpecLikeSpec.scala", 
        "DeprecatedAsyncFeatureSpecLikeSpec2.scala", 
        "DeprecatedAsyncFeatureSpecSpec.scala", 
        "DeprecatedAsyncFeatureSpecSpec2.scala", 
        //"DeprecatedBeforeAndAfterAllProp.scala", // skipped because does not compile yet 
        //"DeprecatedCatchReporterProp.scala", // skipped because does not compile yet 
        //"DeprecatedClassTaggingProp.scala", // skipped because does not compile yet 
        //"DeprecatedFeatureSpecSpec.scala", // skipped because does not compile yet 
        //"DeprecatedParallelTestExecutionInfoExamples.scala", // skipped because does not compile yet 
        //"DeprecatedParallelTestExecutionOrderExamples.scala", // skipped because does not compile yet 
        //"DeprecatedParallelTestExecutionSuiteTimeoutExamples.scala", // skipped because does not compile yet 
        //"DeprecatedParallelTestExecutionTestTimeoutExamples.scala", // skipped because does not compile yet 
        "DeprecatedRandomTestOrderSpec.scala", 
        //"DeprecatedStatusProp.scala", // skipped because does not compile yet 
        //"DeprecatedStopOnFailureProp.scala", // skipped because does not compile yet 
        //"DeprecatedTestDataProp.scala", // skipped because does not compile yet 
        //"DeprecatedTestNameProp.scala", // skipped because does not compile yet 
        //"DirectAssertionsSpec.scala", // skipped because does not compile yet 
        "DispatchReporterSpec.scala", 
        "DocSpecSpec.scala", 
        "EasySuite.scala", 
        "EitherValuesSpec.scala", 
        "EncodedOrderingSpec.scala", 
        "EntrySpec.scala", 
        "EventHelpers.scala", 
        "EveryLoneElementSpec.scala", 
        "EveryShouldContainAllElementsOfLogicalAndSpec.scala", 
        "EveryShouldContainAllElementsOfLogicalOrSpec.scala", 
        //"EveryShouldContainAllElementsOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainAllOfLogicalAndSpec.scala", 
        "EveryShouldContainAllOfLogicalOrSpec.scala", 
        //"EveryShouldContainAllOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainAtLeastOneElementOfLogicalAndSpec.scala", 
        "EveryShouldContainAtLeastOneElementOfLogicalOrSpec.scala", 
        //"EveryShouldContainAtLeastOneElementOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainAtLeastOneOfLogicalAndSpec.scala", 
        "EveryShouldContainAtLeastOneOfLogicalOrSpec.scala", 
        //"EveryShouldContainAtLeastOneOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainAtMostOneElementOfLogicalAndSpec.scala", 
        "EveryShouldContainAtMostOneElementOfLogicalOrSpec.scala", 
        //"EveryShouldContainAtMostOneElementOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainAtMostOneOfLogicalAndSpec.scala", 
        "EveryShouldContainAtMostOneOfLogicalOrSpec.scala", 
        //"EveryShouldContainAtMostOneOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainInOrderElementsOfLogicalAndSpec.scala", 
        "EveryShouldContainInOrderElementsOfLogicalOrSpec.scala", 
        //"EveryShouldContainInOrderElementsOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainInOrderLogicalAndSpec.scala", 
        "EveryShouldContainInOrderLogicalOrSpec.scala", 
        "EveryShouldContainInOrderOnlyLogicalAndSpec.scala", 
        "EveryShouldContainInOrderOnlyLogicalOrSpec.scala", 
        //"EveryShouldContainInOrderOnlySpec.scala", // skipped because does not compile yet 
        //"EveryShouldContainInOrderSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainNoElementsOfLogicalAndSpec.scala", 
        "EveryShouldContainNoElementsOfLogicalOrSpec.scala", 
        //"EveryShouldContainNoElementsOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainNoneOfLogicalAndSpec.scala", 
        "EveryShouldContainNoneOfLogicalOrSpec.scala", 
        //"EveryShouldContainNoneOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainOneElementOfLogicalAndSpec.scala", 
        "EveryShouldContainOneElementOfLogicalOrSpec.scala", 
        //"EveryShouldContainOneElementOfSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainOneOfLogicalAndSpec.scala", 
        "EveryShouldContainOneOfLogicalOrSpec.scala", 
        //"EveryShouldContainOneOfSpec.scala", // skipped because does not compile yet 
        //"EveryShouldContainOnlyLogicalAndSpec.scala", // skipped because tests failed
        //"EveryShouldContainOnlyLogicalOrSpec.scala", // skipped because tests failed 
        //"EveryShouldContainOnlySpec.scala", // skipped because does not compile yet 
        //"EveryShouldContainSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainTheSameElementsAsLogicalAndSpec.scala", 
        "EveryShouldContainTheSameElementsAsLogicalOrSpec.scala", 
        //"EveryShouldContainTheSameElementsAsSpec.scala", // skipped because does not compile yet 
        "EveryShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", 
        "EveryShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", 
        //"EveryShouldContainTheSameElementsInOrderAsSpec.scala", // skipped because does not compile yet 
        "ExampleBeforeAfterParallelSpec.scala", 
        "ExampleParallelSpec.scala", 
        "ExamplesSuite.scala", 
        "ExampleStackSpec.scala", 
        "ExampleSuiteTimeoutSpec.scala", 
        "ExampleTimeoutParallelSpec.scala", 
        "ExpectationHavePropertyMatchers.scala", 
        //"FactSpec.scala", // skipped because does not compile yet 
        "FailureMessagesSuite.scala", 
        //"FilterProp.scala", // skipped because does not compile yet 
        //"FilterSpec.scala", // skipped because does not compile yet 
        "FixtureContextSpec.scala", 
        "FunctionSuiteExamples.scala", 
        "FunctionSuiteProp.scala", 
        "FutureOutcomeSpec.scala", 
        "GivenWhenThenSpec.scala", 
        "InformerSpec.scala", 
        //"InheritedTagProp.scala", // skipped because does not compile yet 
        "InOrderContainMatcherDeciderSpec.scala", 
        "InOrderContainMatcherEqualitySpec.scala", 
        "InOrderContainMatcherSpec.scala", 
        "InOrderElementsOfContainMatcherDeciderSpec.scala", 
        "InOrderElementsOfContainMatcherEqualitySpec.scala", 
        "InOrderElementsOfContainMatcherSpec.scala", 
        "InOrderOnlyContainMatcherDeciderSpec.scala", 
        "InOrderOnlyContainMatcherEqualitySpec.scala", 
        "InOrderOnlyContainMatcherSpec.scala", 
        "InsertionOrderSetSpec.scala", 
        //"InsideMixinSpec.scala", skipped because tests failed 
        //"InsideSpec.scala", skipped because tests failed
        //"InspectorsForMapSpec.scala", skipped because tests failed 
        "InspectorShorthandsRegexWithGroupsSpec.scala", 
        //"InspectorShorthandsSpec.scala", // skipped because does not compile yet 
        //"InspectorsSpec.scala", skipped because tests failed 
        "JavaMapLoneElementSpec.scala", 
        "ListLoneElementSpec.scala", 
        "ListShouldBeEmptyLogicalAndSpec.scala", 
        "ListShouldBeEmptyLogicalOrSpec.scala", 
        "ListShouldBeEmptySpec.scala", 
        "ListShouldContainAllElementsOfLogicalAndSpec.scala", 
        "ListShouldContainAllElementsOfLogicalOrSpec.scala", 
        //"ListShouldContainAllElementsOfSpec.scala", 
        "ListShouldContainAllOfLogicalAndSpec.scala", 
        "ListShouldContainAllOfLogicalOrSpec.scala", 
        //"ListShouldContainAllOfSpec.scala", 
        "ListShouldContainAtLeastOneElementOfLogicalAndSpec.scala", 
        "ListShouldContainAtLeastOneElementOfLogicalOrSpec.scala", 
        //"ListShouldContainAtLeastOneElementOfSpec.scala", 
        "ListShouldContainAtLeastOneOfLogicalAndSpec.scala", 
        "ListShouldContainAtLeastOneOfLogicalOrSpec.scala", 
        //"ListShouldContainAtLeastOneOfSpec.scala", 
        "ListShouldContainAtMostOneElementOfLogicalAndSpec.scala", 
        "ListShouldContainAtMostOneElementOfLogicalOrSpec.scala", 
        //"ListShouldContainAtMostOneElementOfSpec.scala", 
        "ListShouldContainAtMostOneOfLogicalAndSpec.scala", 
        "ListShouldContainAtMostOneOfLogicalOrSpec.scala", 
        //"ListShouldContainAtMostOneOfSpec.scala", 
        "ListShouldContainInOrderElementsOfLogicalAndSpec.scala", 
        "ListShouldContainInOrderElementsOfLogicalOrSpec.scala", 
        //"ListShouldContainInOrderElementsOfSpec.scala", 
        "ListShouldContainInOrderLogicalAndSpec.scala", 
        "ListShouldContainInOrderLogicalOrSpec.scala", 
        "ListShouldContainInOrderOnlyLogicalAndSpec.scala", 
        "ListShouldContainInOrderOnlyLogicalOrSpec.scala", 
        //"ListShouldContainInOrderOnlySpec.scala", 
        //"ListShouldContainInOrderSpec.scala", 
        "ListShouldContainNoElementsOfLogicalAndSpec.scala", 
        "ListShouldContainNoElementsOfLogicalOrSpec.scala", 
        //"ListShouldContainNoElementsOfSpec.scala", 
        "ListShouldContainNoneOfLogicalAndSpec.scala", 
        "ListShouldContainNoneOfLogicalOrSpec.scala", 
        //"ListShouldContainNoneOfSpec.scala", 
        "ListShouldContainOneElementOfLogicalAndSpec.scala", 
        "ListShouldContainOneElementOfLogicalOrSpec.scala", 
        //"ListShouldContainOneElementOfSpec.scala", 
        "ListShouldContainOneOfLogicalAndSpec.scala", 
        "ListShouldContainOneOfLogicalOrSpec.scala", 
        //"ListShouldContainOneOfSpec.scala", 
        //"ListShouldContainOnlyLogicalAndSpec.scala", 
        //"ListShouldContainOnlyLogicalOrSpec.scala", 
        //"ListShouldContainOnlySpec.scala", 
        //"ListShouldContainSpec.scala", // skipped because does not compile yet 
        "ListShouldContainTheSameElementsAsLogicalAndSpec.scala", 
        "ListShouldContainTheSameElementsAsLogicalOrSpec.scala", 
        //"ListShouldContainTheSameElementsAsSpec.scala", 
        "ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", 
        "ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", 
        //"ListShouldContainTheSameElementsInOrderAsSpec.scala", 
        "MapShouldBeDefinedAtSpec.scala", 
        "MatcherGenSpec.scala", 
        //"MatchersSerializableSpec.scala", 
        //"MatchersSpec.scala", 
        //"MatcherStackDepthSpec.scala", 
        //"MethodSuiteExamples.scala", 
        //"MethodSuiteProp.scala", 


        "UnitSpec.scala", 
        "AssertionsSpec.scala",
        "TryValuesSpec.scala", 
        "EitherValuesSpec.scala"
        // "ShouldCompileSpec.scala",
        // "ShouldNotCompileSpec.scala",
        // "ShouldNotTypeCheckSpec.scala"
      )
    ) ++
    /*copyDir("jvm/scalatest-test/src/test/scala/org/scalatest", "org/scalatest", targetDir, 
      List(
        "ShouldCompileSpec.scala",
        "ShouldNotCompileSpec.scala",
        "ShouldNotTypeCheckSpec.scala"
      )
    ) ++*/ 
    copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/expectations", "org/scalatest/expectations", targetDir, 
      List(
        "DirectExpectationsSpec.scala"
      )
    ) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/concurrent", "org/scalatest/concurrent", targetDir,
        List(
          "WaitersSpec.scala",    // skipped because Waiters not supported.
          "AsyncAssertionsSpec.scala",    // skipped because AsyncAssertions (deprecated name for Waiters) not supported.
          "ConductorFixtureSuite.scala",  // skipped because Conductors not supported.
          "ConductorMethodsSuite.scala",   // skipped because Conductors not supported.
          "ConductorSuite.scala",   // skipped because Conductors not supported.
          "ConductorFixtureDeprecatedSuite.scala",  // skipped because Conductors not supported.
          "ConductorMethodsDeprecatedSuite.scala",   // skipped because Conductors not supported.
          "ConductorDeprecatedSuite.scala",   // skipped because Conductors not supported.
          "EventuallySpec.scala",   // skipped because Eventually not supported.
          "IntegrationPatienceSpec.scala",  // skipped because depends on Eventually
          "DeprecatedIntegrationPatienceSpec.scala",
          "JavaFuturesSpec.scala",      // skipped because depends on java futures
          "TestThreadsStartingCounterSpec.scala",   // skipped because depends on Conductors
          "DeprecatedTimeLimitedTestsSpec.scala",   // skipped because DeprecatedTimeLimitedTests not supported.
          "TimeoutsSpec.scala",            // skipped because Timeouts not supported.
          "UltimatelySpec.scala",   // skipped because Eventually not supported.
          "TimeLimitsSpec.scala",  // skipped because failed with line number tests.
          "ScalaFuturesSpec.scala",  // skipped because failed with line number tests.
        )) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/enablers", "org/scalatest/enablers", targetDir, 
        List(
          "PropCheckerAssertingAsyncSpec.scala", // skipped for failing tests.
          "PropCheckerAssertingSpec.scala" // skipped for failing tests.
        )) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/events/examples", "org/scalatest/events/examples", targetDir, 
        List(
          "ExampleCancelSpec.scala", // skipped because does not compile yet.
          "ExampleCancelInNestedSuite.scala" // skipped because does not compile yet.
        )) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/events", "org/scalatest/events", targetDir,
        List(
          "TestLocationJUnit3Suite.scala",
          "TestLocationJUnitSuite.scala",
          "TestLocationTestNGSuite.scala",
          "TestLocationMethodJUnit3Suite.scala",
          "TestLocationMethodJUnitSuite.scala",
          "TestLocationMethodTestNGSuite.scala",
          "LocationMethodSuiteProp.scala", 
          "LocationSuiteProp.scala", // skipped because does not compile yet.
          "ScopePendingProp.scala", // skipped because does not compile yet.
          "LocationSpec.scala",  // skipped because does not compile yet.
          "LocationFunctionSuiteProp.scala", // skipped because does not compile yet.
          "EventSpec.scala", // skipped because does not compile yet.
          "DeprecatedScopePendingProp.scala",  // skipped because does not compile yet.
          "DeprecatedLocationSuiteProp.scala", // skipped because does not compile yet.
          "DeprecatedLocationFunctionSuiteProp.scala" // skipped because does not compile yet.
        )) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/exceptions", "org/scalatest/exceptions", targetDir, 
        List(
          "StackDepthExceptionSpec.scala", // skipped because does not compile yet.
          "PayloadSpec.scala" // skipped because does not compile yet.
        )) ++
      /*copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/fixture", "org/scalatest/fixture", targetDir,
        List(
          "SpecSpec.scala",     // skipped because depends on java reflections
          "SuiteSpec.scala"    // skipped because depends on java reflections
        )) ++ */
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/path", "org/scalatest/path", targetDir, 
        List(
          "StackSpec.scala",  // skipped because does not compile yet.
          "FunSpecSpec.scala",  // skipped because does not compile yet.
          "FreeSpecSpec.scala" // skipped because does not compile yet.
        )) ++
      /*copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/prop", "org/scalatest/prop", targetDir, List.empty) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/suiteprop", "org/scalatest/suiteprop", targetDir, List.empty) ++*/
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/matchers", "org/scalatest/matchers", targetDir, 
        List(
          "TypeMatcherMacroSpec.scala", // skipped because does not compile yet.
          "MatcherProducersSpec.scala" // skipped because does not compile yet.
        )
      ) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/time", "org/scalatest/time", targetDir, 
        List(
          "SpanSugarSpec.scala" // skipped because does not compile yet.
        )
      ) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/verbs", "org/scalatest/verbs", targetDir, List.empty) ++
      copyDir("jvm/scalatest-test/src/test/scala/org/scalatest/tools", "org/scalatest/tools", targetDir,
        List(
          "DiscoverySuiteSuite.scala",  // skipped because failing test.
          "FilterReporterSpec.scala",  // skipped because does not compile yet.
          "FrameworkSuite.scala", // skipped because hang when tests execute.
          "ScalaTestFrameworkSuite.scala", // skipped because does not compile yet.
          "ScalaTestRunnerSuite.scala", // skipped because does not compile yet.
          "SuiteDiscoveryHelperSuite.scala",  // skipped because does not compile yet.
          "XmlSocketReporterSpec.scala", // skipped because tests failed execute.
          "SuiteSortingReporterSpec.scala",  // skipped because does not compile yet.
          "TestSortingReporterSpec.scala" // skipped because does not compile yet.
        )
      )
    }

    def genDiagramsTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
      copyDir("jvm/diagrams-test/src/test/scala/org/scalatest/diagrams", "org/scalatest/diagrams", targetDir, 
        List(
          "DiagramsSpec.scala", // skipped because tests failed execute.
          "DirectDiagrammedAssertionsSpec.scala" // skipped because tests failed execute.
        )
      )

    def genFeatureSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
      copyDir("jvm/featurespec-test/src/test/scala/org/scalatest/featurespec", "org/scalatest/featurespec", targetDir, List.empty)

    def genFlatSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
      copyDir("jvm/flatspec-test/src/test/scala/org/scalatest/flatspec", "org/scalatest/flatspec", targetDir, 
        List(
          "FlatSpecImportedMatchersSpec.scala"
        )
      )    

    def genFreeSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
      copyDir("jvm/freespec-test/src/test/scala/org/scalatest/freespec", "org/scalatest/freespec", targetDir, List.empty)

    def genFunSpecTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
      copyDir("jvm/funspec-test/src/test/scala/org/scalatest/funspec", "org/scalatest/funspec", targetDir, List.empty)    

    def genFunSuiteTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = 
      copyDir("jvm/funsuite-test/src/test/scala/org/scalatest/funsuite", "org/scalatest/funsuite", targetDir, 
        List("FunSuiteSpec.scala")
      )
}
