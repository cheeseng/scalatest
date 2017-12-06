/*
 * Copyright 2001-2016 Artima, Inc.
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
package org.scalactic.source

import org.scalactic.ScalacticVersions

trait ObjectMeta {

  def fieldNames: scala.collection.immutable.IndexedSeq[String]

  def hasField(name: String): Boolean = fieldNames.contains(name)

  def value(name: String): Any

  def typeName(name: String): String

  def shortTypeName(name: String): String

}

object ObjectMeta {

  import reflect.runtime.universe._

  def apply(v: Any): ObjectMeta = {

    /*new ObjectMeta {

      val typeMirror = runtimeMirror(v.getClass.getClassLoader)
      val instanceMirror = typeMirror.reflect(v)
      val symbol: ClassSymbol = instanceMirror.symbol

      def mapCaseAccessor(s: Symbol): Option[String] = {
        s match {
          case m: MethodSymbol =>
            if (m.isCaseAccessor)
              Some(s.name.toString)
            else
              None

          case m: TermSymbol if m.isVal =>
            val name = m.name.toString.trim
            if (m.name.toString.endsWith("$mcI$sp"))
              Some(name.substring(0, name.length - 7))
            else
              Some(name)

          case other =>
            None
        }
      }

      val caseAccessorSymbols = symbol.toType.declarations

      lazy val fieldNames = {
        symbol.toType.declarations.flatMap(mapCaseAccessor).toVector.distinct
      }

      def value(name: String): Any = {
        caseAccessorSymbols.find(s => (s.name.toString == name || s.name.toString == (name + "$mcI$sp")) && (s.isMethod || s.isTerm)) match {
          case Some(fieldSymbol) =>
            val fieldMirror = instanceMirror.reflectField(fieldSymbol.asTerm)
            fieldMirror.get

          case None =>
            throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
        }
      }

      def typeName(name: String): String = {
        caseAccessorSymbols.find(s => s.name.toString == name) match {
          case Some(fieldSymbol) =>
            // This is a safe cast
            fieldSymbol.asInstanceOf[MethodSymbol].returnType.typeSymbol.fullName

          case None =>
            throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
        }
      }

      def shortTypeName(name: String): String = {
        caseAccessorSymbols.find(s => s.name.toString == name) match {
          case Some(fieldSymbol) =>
            // This is a safe cast
            fieldSymbol.asInstanceOf[MethodSymbol].returnType.typeSymbol.name.decoded

          case None =>
            throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
        }
      }
    }*/

    def mapCaseAccessor(s: Symbol): Option[String] = {
      s match {
        case m: MethodSymbol =>
          if (m.isCaseAccessor)
            Some(s.name.toString)
          else
            None

        case m: TermSymbol if m.isVal =>
          val name = m.name.toString.trim
          if (m.name.toString.endsWith("$mcI$sp"))
            Some(name.substring(0, name.length - 7))
          else
            Some(name)

        case other =>
          None
      }
    }

    // Scala reflection in scala 2.10 is not thread safe, and not reliable to use, see:
    // https://issues.scala-lang.org/browse/SI-6240
    // so for scala 2.10 we'll synchronized the code to call scala reflection and build the fields
    // and values map eagerly.
    if (ScalacticVersions.BuiltForScalaVersion == "2.10") {
      val (fieldNameList, valueMap, typeNameMap, shortTypeNameMap) =
        synchronized {
          val typeMirror = runtimeMirror(v.getClass.getClassLoader)
          val instanceMirror = typeMirror.reflect(v)
          val symbol: ClassSymbol = instanceMirror.symbol

          val caseAccessorSymbols = symbol.toType.declarations

          val fieldNames: Vector[String] = {
            symbol.toType.declarations.flatMap(mapCaseAccessor).toVector.distinct
          }

          val valueMap: Map[String, Any] =
            Map.empty[String, Any] ++
              fieldNames.map { name =>
                caseAccessorSymbols.find(s => (s.name.toString == name || s.name.toString == (name + "$mcI$sp")) && (s.isMethod || s.isTerm)) match {
                  case Some(fieldSymbol) =>
                    val fieldMirror = instanceMirror.reflectField(fieldSymbol.asTerm)
                    (name, fieldMirror.get)

                  case None =>
                    throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
                }
              }

          val typeNameMap: Map[String, String] =
            Map.empty[String, String] ++
              fieldNames.map { name =>
                caseAccessorSymbols.find(s => s.name.toString == name) match {
                  case Some(fieldSymbol) =>
                    // This is a safe cast
                    (name, fieldSymbol.asInstanceOf[MethodSymbol].returnType.typeSymbol.fullName)
                    //(name, fieldSymbol.asInstanceOf[TermSymbol].asType.fullName)

                  case None =>
                    throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
                }
              }

          val shortTypeNameMap: Map[String, String] =
            Map.empty[String, String] ++
              fieldNames.map { name =>
                caseAccessorSymbols.find(s => s.name.toString == name) match {
                  case Some(fieldSymbol) =>
                    // This is a safe cast
                    (name, fieldSymbol.asInstanceOf[MethodSymbol].returnType.typeSymbol.name.decoded)

                  case None =>
                    throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
                }
              }

          (fieldNames, valueMap, typeNameMap, shortTypeNameMap)
        }

      new ObjectMeta {

        val fieldNames = fieldNameList

        def value(name: String): Any =
          valueMap.get(name) match {
            case Some(value) => value
            case None =>
              throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
          }

        def typeName(name: String): String =
          typeNameMap.get(name) match {
            case Some(value) => value
            case None =>
              throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
          }

        def shortTypeName(name: String): String =
          shortTypeNameMap.get(name) match {
            case Some(value) => value
            case None =>
              throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
          }
      }
    }
    else
      new ObjectMeta {

        val typeMirror = runtimeMirror(v.getClass.getClassLoader)
        val instanceMirror = typeMirror.reflect(v)
        val symbol: ClassSymbol = instanceMirror.symbol

        val caseAccessorSymbols = symbol.toType.declarations

        lazy val fieldNames = {
          symbol.toType.declarations.flatMap(mapCaseAccessor).toVector.distinct
        }

        def value(name: String): Any = {
          caseAccessorSymbols.find(s => (s.name.toString == name || s.name.toString == (name + "$mcI$sp")) && (s.isMethod || s.isTerm)) match {
            case Some(fieldSymbol) =>
              val fieldMirror = instanceMirror.reflectField(fieldSymbol.asTerm)
              fieldMirror.get

            case None =>
              throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
          }
        }

        def typeName(name: String): String = {
          caseAccessorSymbols.find(s => s.name.toString == name) match {
            case Some(fieldSymbol) =>
              // This is a safe cast
              fieldSymbol.asInstanceOf[MethodSymbol].returnType.typeSymbol.fullName

            case None =>
              throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
          }
        }

        def shortTypeName(name: String): String = {
          caseAccessorSymbols.find(s => s.name.toString == name) match {
            case Some(fieldSymbol) =>
              // This is a safe cast
              fieldSymbol.asInstanceOf[MethodSymbol].returnType.typeSymbol.name.decoded

            case None =>
              throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
          }
        }
      }

  }

}