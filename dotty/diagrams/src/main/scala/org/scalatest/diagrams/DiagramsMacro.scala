/*
 * Copyright 2001-2012 Artima, Inc.
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
package org.scalatest.diagrams

import org.scalactic._
import scala.quoted._
import org.scalatest.Assertions
import org.scalatest.compatible.Assertion

object DiagramsMacro {
  // Transform the input expression by parsing out the anchor and generate expression that can support diagram rendering
  def parse(using Quotes)(expr: quotes.reflect.Term): quotes.reflect.Term = {
    import quotes.reflect._
    import util._
    import ValDef.let

    expr.tpe.asType match {
      case '[r] =>

        // Generate AST for:
        // val name = rhs
        def valDef(name: String, rhs: Term): ValDef = { //https://eed3si9n.com/intro-to-scala-3-macros/
          val sym = Symbol.newVal(
            Symbol.spliceOwner,
            name, 
            rhs.tpe, 
            Flags.EmptyFlags,
            Symbol.noSymbol,
          )
          ValDef(sym, Some(rhs))
        }

        def isXmlSugar(apply: Apply): Boolean = apply.tpe <:< TypeRepr.of[scala.xml.Elem]
        def isJavaStatic(tree: Tree): Boolean = tree.symbol.flags.is(Flags.Static)
        def isImplicitMethodType(tp: TypeRepr): Boolean = tp match {
          case tp: MethodType => tp.isImplicit
          case _ => false
        }

        def selectField(o: Term, name: String): Term = Select.unique(o, name)

        def default(term: Term): Term = term.asExpr match {
          case '{ $x: t } => '{ DiagrammedExpr.simpleExpr[t]($x, ${ getAnchor(term) } ) }.asTerm
        }

        def byNameExpr(term: Term): Term = term.asExpr match {
          case '{ $x: t } => '{ DiagrammedExpr.byNameExpr[t]($x, ${ getAnchor(term) } ) }.asTerm
        }

        def xmlSugarExpr(term: Term): Term = term.asExpr match {
          case '{ $x: t } => '{ 
            DiagrammedExpr.simpleExpr[t]($x, ${ 
              // https://docs.scala-lang.org/scala3/reference/metaprogramming/reflection.html#positions
              val anchor = expr.pos.startColumn - Position.ofMacroExpansion.startColumn
              val c = expr.pos.sourceCode.getOrElse("<none>").head
              Expr(anchor - (if (c == '<') 0 else 1)) 
            } ) 
          }.asTerm
        }

        def getAnchorForSelect(sel: Select): Expr[Int] = {
          if (sel.name == "unary_!")
            Expr(sel.pos.startColumn - Position.ofMacroExpansion.startColumn)
          else {
            val selOffset = sel.pos.endColumn - sel.qualifier.pos.endColumn - sel.name.length
            Expr(sel.qualifier.pos.endColumn + selOffset - Position.ofMacroExpansion.startColumn)
          }
        }

        def getAnchor(expr: Term): Expr[Int] = {
          // -1 to match scala2 position
          // Expr((expr.asTerm.pos.endColumn + expr.asTerm.pos.startColumn - 1) / 2 - Position.ofMacroExpansion.startColumn)
          Expr(expr.pos.startColumn - Position.ofMacroExpansion.startColumn)
        }

        def handleArgs(argTps: List[TypeRepr], args: List[Term]): (List[Term], List[Term]) = {
          println("####args: " + args.map(_.show).mkString("======================", "\n" ,"======================"))
          println("####argTps: " + argTps.map(_.isInstanceOf[ByNameType]).mkString("======================", "\n" ,"======================"))
          args.zip(argTps).foldLeft(Nil -> Nil : (List[Term], List[Term])) { case ((diagrams, others), pair) =>
            pair match {
              case (Typed(Repeated(args, _), _), AppliedType(_, _)) =>
                (diagrams :++ args.map(parse), others)
              case (arg, ByNameType(_)) =>
                println("####Found by name: " + arg.show)
                (diagrams, others :+ byNameExpr(arg))
              case (arg, tp) =>
                if (tp.widen.typeSymbol.fullName.startsWith("scala.Function")) (diagrams, others :+ /*byNameExpr(arg)*/arg)
                else (diagrams :+ parse(arg), others)
            }
          }
        }

        println("####expr: " + expr.show + ", ---- " + expr.getClass.getName)

        expr match {
          case apply: Apply if isXmlSugar(apply) => println("###1"); xmlSugarExpr(expr)

          case Apply(Select(New(_), _), _) => println("###2"); default(expr)

          case apply: Apply if isJavaStatic(apply) => println("###3"); default(expr)

          case Select(This(_), _) => println("###4"); default(expr)

          case x: Select if x.symbol.flags.is(Flags.Module) => println("###5"); default(expr)

          case x: Select if isJavaStatic(x) => println("###6"); default(expr)

          case sel @ Select(qual, name) =>
             println("###7");
            parse(qual).asExpr match {
              case '{ $obj: DiagrammedExpr[t] } =>
                val anchor = getAnchorForSelect(sel)
                '{
                  val o = $obj
                  DiagrammedExpr.selectExpr[r](o, ${ selectField('{o.value}.asTerm, name).asExprOf[r] }, $anchor)
                }.asTerm
            }

          case Block(stats, bexpr) =>
             println("###8");
            // call parse recursively using the expr argument if it is a block
            //Block(stats, parse(bexpr))
            default(expr)

          case Apply(sel @ Select(lhs, op), rhs :: Nil) =>
            println("###9");
            val anchor = getAnchorForSelect(sel)
            op match {
              case "||" | "|" =>
                println("###9.1")
                val left = parse(lhs).asExprOf[DiagrammedExpr[Boolean]]
                val right = parse(rhs).asExprOf[DiagrammedExpr[Boolean]]

                '{
                  val l = $left
                  if (l.value) l
                  else {
                    val r = $right
                    DiagrammedExpr.applyExpr[Boolean](l, r :: Nil, r.value, $anchor)
                  }
                }.asTerm
              case "&&" | "&" =>
                println("###9.2")
                val left = parse(lhs).asExprOf[DiagrammedExpr[Boolean]]
                val right = parse(rhs).asExprOf[DiagrammedExpr[Boolean]]
                '{
                  val l = $left
                  if (!l.value) l
                  else {
                    val r = $right
                    DiagrammedExpr.applyExpr[Boolean](l, r :: Nil, r.value, $anchor)
                  }
                }.asTerm
              case _ =>
                println("###9.3")
                val left = parse(lhs)

                val methTp = sel.tpe.widen.asInstanceOf[MethodType]
                println("*****9.4: " + methTp.paramTypes)
                val (diagrams, others) = handleArgs(methTp.paramTypes, rhs :: Nil)
                println("*****9.5: " + diagrams.map(_.show).mkString("\n"))
                println("*****9.6: " + others.map(_.show).mkString("\n"))

                let(Symbol.spliceOwner, left) { l =>
                  let(Symbol.spliceOwner, diagrams) { rs =>
                    l.asExpr match {
                      case '{ $left: DiagrammedExpr[t] } =>
                        val rights = rs.map(_.asExprOf[DiagrammedExpr[_]])
                        val res = Select.overloaded(Select.unique(l, "value"), op, Nil, diagrams.map(r => Select.unique(r, "value")) ++ others.map(r => Select.unique(r, "value"))).asExprOf[r]
                        '{ DiagrammedExpr.applyExpr[r]($left, ${Expr.ofList(rights)}, $res, $anchor) }.asTerm
                    }
                  }
                }
            }

          case Apply(sel @ Select(lhs, op), args) =>
            println("###10");
            val left = parse(lhs)
            val anchor = getAnchorForSelect(sel)

            val methTp = sel.tpe.widen.asInstanceOf[MethodType]
            println("*****2" + methTp.paramTypes)
            val (diagrams, others) = handleArgs(methTp.paramTypes, args)

            let(Symbol.spliceOwner, left) { l =>
              let(Symbol.spliceOwner, diagrams) { rs =>
                let(Symbol.spliceOwner, others) { os =>
                  l.asExpr match {
                    case '{ $left: DiagrammedExpr[t] } =>
                      val rights = rs.map(_.asExprOf[DiagrammedExpr[_]])
                      val res = Select.overloaded(Select.unique(l, "value"), op, Nil, rs.map(r => Select.unique(r, "value")) ++ os.map(r => Select.unique(r, "value"))).asExprOf[r]
                      '{ DiagrammedExpr.applyExpr[r]($left, ${Expr.ofList(rights)}, $res, $anchor) }.asTerm
                  }
                }
              }
            }    

          case Apply(f @ Apply(sel @ Select(Apply(qual, lhs :: Nil), op @ ("===" | "!==")), rhs :: Nil), implicits)
          if isImplicitMethodType(f.tpe) =>
            println("###11");
            val left = parse(lhs)
            val right = parse(rhs)
            val anchor = getAnchorForSelect(sel)

            let(Symbol.spliceOwner, left) { left =>
              let(Symbol.spliceOwner, right) { right =>
                val app = qual.appliedTo(Select.unique(left, "value")).select(sel.symbol)
                              .appliedTo(Select.unique(right, "value")).appliedToArgs(implicits)
                let(Symbol.spliceOwner, app) { result =>
                  val l = left.asExprOf[DiagrammedExpr[_]]
                  val r = right.asExprOf[DiagrammedExpr[_]]
                  val b = result.asExprOf[Boolean]
                  '{ DiagrammedExpr.applyExpr[Boolean]($l, $r :: Nil, $b, $anchor) }.asTerm
                }
              }
            }

          case Apply(fun @ TypeApply(sel @ Select(lhs, op), targs), args) =>
            println("###12");
            val left = parse(lhs)
            val anchor = getAnchorForSelect(sel)

            val methTp = fun.tpe.widen.asInstanceOf[MethodType]
            println("*****3: " + methTp.paramTypes)
            val (diagrams, others) = handleArgs(methTp.paramTypes, args)

            let(Symbol.spliceOwner, left) { l =>
              let(Symbol.spliceOwner, diagrams) { rs =>
                let(Symbol.spliceOwner, others) { os =>
                  l.asExpr match {
                    case '{ $left: DiagrammedExpr[t] } =>
                      val rights = rs.map(_.asExprOf[DiagrammedExpr[_]])
                      val result = Select.overloaded(Select.unique(l, "value"), op, targs.map(_.tpe), rs.map(r => Select.unique(r, "value")) ++ os.map(r => Select.unique(r, "value")))
                      println("+++++++++++++++++++++++++: " + result.show)
                      println("####after+++++1")
                      let(Symbol.spliceOwner, result) { resu =>
                        println("####after+++++2: " + result.show)
                        val res = resu.asExprOf[r]
                        println("####after+++++3")
                        //val res = Select.overloaded(Select.unique(l, "value"), op, targs.map(_.tpe), diagrams.map(r => Select.unique(r, "value")) ++ others/*.map(r => Select.unique(r, "value"))*/.map(r => Apply(Select.unique(Select.unique(r, "value"), "apply"), List.empty))).asExprOf[r]
                        println("####after+++++4")
                        '{ DiagrammedExpr.applyExpr[r]($left, ${Expr.ofList(rights)}, $res, $anchor) }.asTerm
                      }
                  }
                }
              }
            }

          case TypeApply(sel @ Select(lhs, op), targs) =>
            println("###13");
            val left = parse(lhs)
            val anchor = getAnchorForSelect(sel)

            let(Symbol.spliceOwner, left) { l =>
              l.asExpr match {
                case '{ $left: DiagrammedExpr[t] } =>
                  val res = Select.unique(l, "value").select(sel.symbol).appliedToTypes(targs.map(_.tpe)).asExprOf[r]
                  '{ DiagrammedExpr.applyExpr[r]($left, Nil, $res, $anchor) }.asTerm
              }
            }

          case Apply(Apply(fun @ TypeApply(sel @ Select(lhs, op), targs), args), args2) =>
            println("**************apply of apply: " + expr)
            val left = parse(lhs)
            val anchor = getAnchorForSelect(sel)
            val methTp = fun.tpe.widen.asInstanceOf[MethodType]
            val (diagrams, others) = handleArgs(methTp.paramTypes, args)
            let(Symbol.spliceOwner, left) { l =>
              let(Symbol.spliceOwner, diagrams) { rs =>
                let(Symbol.spliceOwner, others) { os =>
                  l.asExpr match {
                    case '{ $left: DiagrammedExpr[t] } =>
                      val rights = rs.map(_.asExprOf[DiagrammedExpr[_]])
                      val result = Apply(Select.overloaded(Select.unique(l, "value"), op, targs.map(_.tpe), rs.map(r => Select.unique(r, "value")) ++ os.map(r => Select.unique(r, "value"))), args2)
                      let(Symbol.spliceOwner, result) { resu =>
                        val res = resu.asExprOf[r]
                        '{ DiagrammedExpr.applyExpr[r]($left, ${Expr.ofList(rights)}, $res, $anchor) }.asTerm
                      }
                  }
                }
              }
            }
            // TODO: continue copy code from ###12
            default(expr)  

          case other =>
            println("###15: " + other.show + ", ast: " + other);
            default(expr)
        }
    }
  }

  def transform(
    helper: Expr[(DiagrammedExpr[Boolean], Any, String, source.Position) => Assertion],
    condition: Expr[Boolean], pos: Expr[source.Position], clue: Expr[Any], sourceText: String
  )(using Quotes): Expr[Assertion] = {
    import quotes.reflect._
    val diagExpr = parse(condition.asTerm.underlyingArgument).asExprOf[DiagrammedExpr[Boolean]]
    println("###debug: " + diagExpr.show)

    /*
    public java.lang.String scala.quoted.runtime.impl.ExprImpl.toString()
public dotty.tools.dotc.ast.Trees$Tree scala.quoted.runtime.impl.ExprImpl.tree()
public scala.quoted.runtime.impl.Scope scala.quoted.runtime.impl.ExprImpl.scope()
public static scala.quoted.Expr scala.quoted.Expr.apply(java.lang.Object,scala.quoted.ToExpr,scala.quoted.Quotes)
public static scala.quoted.Expr scala.quoted.Expr.block(scala.collection.immutable.List,scala.quoted.Expr,scala.quoted.Quotes)
public static scala.quoted.Expr scala.quoted.Expr.ofList(scala.collection.immutable.Seq,scala.quoted.Type,scala.quoted.Quotes)
public static scala.quoted.Expr scala.quoted.Expr.ofSeq(scala.collection.immutable.Seq,scala.quoted.Type,scala.quoted.Quotes)
public static scala.quoted.Expr scala.quoted.Expr.ofTuple(scala.Product,scala.$eq$colon$eq,scala.quoted.Type,scala.quoted.Quotes)
public static scala.quoted.Expr scala.quoted.Expr.ofTupleFromSeq(scala.collection.immutable.Seq,scala.quoted.Quotes)
public static scala.Option scala.quoted.Expr.summon(scala.quoted.Type,scala.quoted.Quotes)
public static scala.quoted.Expr scala.quoted.Expr.betaReduce(scala.quoted.Expr,scala.quoted.Quotes)
public static scala.Option scala.quoted.Expr.unapply(scala.quoted.Expr,scala.quoted.FromExpr,scala.quoted.Quotes)
public final native void java.lang.Object.wait(long) throws java.lang.InterruptedException
public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException
public final void java.lang.Object.wait() throws java.lang.InterruptedException
public native int java.lang.Object.hashCode()
public final native java.lang.Class java.lang.Object.getClass()
public final native void java.lang.Object.notify()
public final native void java.lang.Object.notifyAll()

    */
    '{ $helper($diagExpr, $clue, ${Expr(sourceText)}, $pos) }
  }
}