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
package org.scalactic.source

import scala.quoted.*

trait Position {
  def fileName: String 
  def filePathname: String 
  def lineNumber: Int
}

/**
 * A source file position consisting of a simple source file name, the
 * full path to the file, and a line number.
 *
 * <p>
 * Class <code>Position</code> is used by ScalaTest assertions, matchers,
 * and testing styles to provide locations for failures in test results.
 * </p>
 *
 * <p>
 * The <code>pathFilename<code> should always end with the <code>fileName</code>, which
 * is the simple file name. For example, if the <code>filePathname</code> were <code>"/Users/this/is/a/class/Act.scala"</code>,
 * the <code>fileName</code> would be <code>"Act.scala"</code>.
 * </p>
 *
 * <p>
 * <em>Note: Class <code>Position</code> is in part inspired by the <a href="https://github.com/lihaoyi/sourcecode" target="_blank"><code>sourcecode</code></a> library designed by Li Haoyi.</a></em>
 * </p>
 *
 * @param fileName the simple name of a source file
 * @param filePathname the fully qualified pathname of the source file
 * @param lineNumber a line number inside the source file with the given filePathname and fileNamae
 */
case class SelfPosition(fileName: String, filePathname: String, lineNumber: Int) extends Position

case class ParentPosition(fileName: String, filePathname: String, lineNumber: Int) extends Position

/**
 * Companion object for <code>Position</code> that defines an implicit
 * method that uses a macro to grab the enclosing position.
 */
object Position {

  import org.scalactic.Resources

  def apply(fileName: String, filePathname: String, lineNumber: Int): Position = new SelfPosition(fileName, filePathname, lineNumber)

  /**
   * Inline given method, implemented with a macro, that returns the enclosing
   * source position where it is invoked.
   *
   * @return the enclosing source position
   */
  //inline given here: Position = ${ genPosition }

  inline given ParentPosition = ${ genParentPosition }

  private[scalactic] lazy val showScalacticFillFilePathnames: Boolean = 
    Option(System.getenv("SCALACTIC_FILL_FILE_PATHNAMES")) == Some("yes")

  private[org] def filePathnames(path: String): String = 
    if (showScalacticFillFilePathnames) path else Resources.pleaseDefineScalacticFillFilePathnameEnvVar

  /**
   * Helper method for Position macro.
   */
  private def genPosition(using Quotes): Expr[Position] = {
    val pos = quotes.reflect.Position.ofMacroExpansion
    val file = pos.sourceFile
    val fileName: String = Option(file.jpath).map(_.getFileName.toString).getOrElse("<unknown>")
    val filePath: String = filePathnames(file.toString)
    val lineNo: Int = pos.startLine + 1
    '{ Position(${Expr(fileName)}, ${Expr(filePath)}, ${Expr(lineNo)}) }
  }

  def withPosition[T](fun: Expr[Position => T])(using quotes: Quotes, typeOfT: Type[T]): Expr[T] = {
    val pos = quotes.reflect.Position.ofMacroExpansion
    val file = pos.sourceFile
    val fileName: String = Option(file.jpath).map(_.getFileName.toString).getOrElse("<unknown>")
    val filePath: String = org.scalactic.source.Position.filePathnames(file.toString)
    val lineNo: Int = pos.startLine + 1
   '{${fun}.apply(org.scalactic.source.Position(${Expr(fileName)}, ${Expr(filePath)}, ${Expr(lineNo)}))}
  }

  private def genParentPosition(using Quotes): Expr[ParentPosition] = {
    import quotes.reflect.*
    
    // Get the macro expansion position
    val macroPos = quotes.reflect.Position.ofMacroExpansion
    
    // This accumulator finds the parent of the tree at the macro position
    class ParentFinder extends TreeAccumulator[Option[Tree]] {
      def foldTree(parent: Option[Tree], tree: Tree)(owner: Symbol): Option[Tree] = {
        val treePos = tree.pos
        println("=========treePos: " + treePos)
        
        // Check if this exact tree is at the macro expansion position
        if (treePos.start == macroPos.start && treePos.end == macroPos.end) {
          // Found the exact node - return its parent
          println("##########parent: " + parent)
          parent
        } else if (treePos.start <= macroPos.start && macroPos.end <= treePos.end) {
          // This tree contains the macro position, search in its children
          // Pass 'Some(tree)' as the parent for the children
          foldOverTree(Some(tree), tree)(owner)
        } else {
          // This tree doesn't contain the macro position
          parent
        }
      }
    }
    
    val rootTree = Symbol.spliceOwner.tree
    println("=========rootTree: " + rootTree)
    val parentTreeOpt = new ParentFinder().foldTree(None, rootTree)(Symbol.spliceOwner)
    
    val pos = parentTreeOpt.map(_.pos).getOrElse(macroPos)
    
    val file = pos.sourceFile
    val fileName: String = Option(file.jpath).map(_.getFileName.toString).getOrElse("<unknown>")
    val filePath: String = filePathnames(file.toString)
    val lineNo: Int = pos.startLine + 1
    
    '{ ParentPosition(${Expr(fileName)}, ${Expr(filePath)}, ${Expr(lineNo)}) }
  }

}

