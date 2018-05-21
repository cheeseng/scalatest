/*
 * Copyright 2001-2018 Artima, Inc.
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
package org.scalatest

import scala.tools.nsc.GenericRunnerSettings
import scala.tools.nsc.interpreter.IMain

class Compiler {

  def compile(code: String): Boolean = {
    val interpreter = {
      val out = System.out
      val flusher = new java.io.PrintWriter(out)
      val settings = new GenericRunnerSettings( println _ )
      //settings.usejavacp.value = true
      //settings.classpath.value = "/home/cheeseng/.ivy2/cache/org.scala-lang/scala-compiler/jars/scala-compiler-2.12.4.jar:/home/cheeseng/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.4.jar:/home/cheeseng/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.12.4.jar:/home/cheeseng/.ivy2/cache/org.scala-lang.modules/scala-xml_2.12/bundles/scala-xml_2.12-1.0.4.jar:/home/cheeseng/.ivy2/cache/org.scala-lang.modules/scala-parser-combinators_2.12/bundles/scala-parser-combinators_2.12-1.0.4.jar"
      val classpath = {
        //java.lang.Thread.currentThread.getContextClassLoader match {
        getClass.getClassLoader match {
          case cl: java.net.URLClassLoader => cl.getURLs.toList map {_.toString}
          case x =>
            // try one level up before giving up
            x.getParent match {
              case cl: java.net.URLClassLoader => cl.getURLs.toList map {_.toString}
              case x => sys.error("classloader is not URLClassLoader: " + x.getClass)
            }
        }
      }
      println("###classpath: " + classpath)
      settings.classpath.value = classpath.mkString(java.io.File.pathSeparator)
      new IMain(settings, flusher)
    }
    val result = interpreter.interpret(code)

    if (result == scala.tools.nsc.interpreter.IR.Success)
      true
    else
      false
  }

}