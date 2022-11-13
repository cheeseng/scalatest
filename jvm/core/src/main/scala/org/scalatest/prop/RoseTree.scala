/*
 * Copyright 2001-2020 Artima, Inc.
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

import scala.annotation.tailrec
import scala.concurrent.{Future, ExecutionContext}

trait RoseTree[T] { thisRoseTreeOfT =>

  val value: T

  // Compute the shrinks list on demand using this RoseTree's value.
  // This will be called only when a property fails, and just once, and it
  // won't take long, so no need to make this a lazy val.
  def shrinks(rnd: Randomizer): (List[RoseTree[T]], Randomizer)

  def depthFirstShrinks[E](fun: T => (Boolean, Option[E]), rnd: Randomizer): (List[RoseTree[T]], Option[E], Randomizer) = {
    @tailrec
    def shrinkLoop(lastFailure: RoseTree[T], lastFailureData: Option[E], pending: List[RoseTree[T]], processed: Set[T] , currentRnd: Randomizer): (List[RoseTree[T]], Option[E], Randomizer) = {
      pending match {
        case head :: tail => 
          val (result, errDataOpt) = fun(head.value)
          if (!result) {
            // If the function fail, we got a new failure value, and we'll go one level deeper.
            val (headChildrenRTs, nextRnd) = head.shrinks(currentRnd)
            val newProceesed = processed + head.value
            shrinkLoop(head, errDataOpt, headChildrenRTs.filter(rt => !newProceesed.contains(rt.value)), newProceesed,  nextRnd)
          }
          else {
            // The function call succeeded, let's continue to try the sibling.
            // Use view to be 'lazy'
            tail.view.map(rt => (rt, fun(rt.value)))
                     .find(!_._2._1) match {
              case Some((rt, (result, errDataOpt))) =>
                // Sibling with counter example found, we'll continue from sibling.
                val (rtChildrenRTs, nextRnd) = rt.shrinks(currentRnd)
                val newProceesed = processed + head.value + rt.value
                shrinkLoop(rt, errDataOpt, rtChildrenRTs.filter(c => !newProceesed.contains(c.value)), newProceesed, nextRnd)
              case None => 
                // No sibling with counter example found, we'll continue to look from half.
                val (headChildrenRTs, nextRnd) = head.shrinks(currentRnd)
                val newProceesed = processed + head.value
                shrinkLoop(lastFailure, errDataOpt, headChildrenRTs.filter(rt => !newProceesed.contains(rt.value)), newProceesed,  nextRnd)
            }
          }

        case Nil => // No more further sibling to try, return the last failure
          (List(lastFailure), lastFailureData, currentRnd)
      }
    }
    val (firstLevelShrinks, nextRnd) = shrinks(rnd)
    shrinkLoop(this, None, firstLevelShrinks, Set(value), nextRnd)
  }

  def depthFirstShrinksForFuture[E](fun: T => Future[(Boolean, Option[E])], rnd: Randomizer)(implicit execContext: ExecutionContext): Future[(List[RoseTree[T]], Option[E], Randomizer)] = {
    def shrinkLoop(lastFailure: RoseTree[T], lastFailureData: Option[E], pending: List[RoseTree[T]], processed: Set[T] , currentRnd: Randomizer): Future[(List[RoseTree[T]], Option[E], Randomizer)] = {
      pending match {
        case head :: tail => 
          val future = fun(head.value)
          future.flatMap { case (result, errDataOpt) =>
            if (!result) {
              // If the function fail, we got a new failure value, and we'll go one level deeper.
              val (headChildrenRTs, nextRnd) = head.shrinks(currentRnd)
              val newProceesed = processed + head.value
              shrinkLoop(head, errDataOpt, headChildrenRTs.filter(rt => !newProceesed.contains(rt.value)), newProceesed,  nextRnd)
            }
            else {
              // The function call succeeded, let's continue to try the sibling.
              shrinkLoop(lastFailure, lastFailureData, tail, processed + head.value, currentRnd)
            }
          }

        case Nil =>
          Future.successful((List(lastFailure), lastFailureData, currentRnd))
      }
    }

    val (firstLevelShrinks, nextRnd) = shrinks(rnd)
    shrinkLoop(this, None, firstLevelShrinks, Set(value), nextRnd)
  }

  def combineFirstDepthShrinks[E, U](fun: (T, U) => (Boolean, Option[E]), rnd: Randomizer, roseTreeOfU: RoseTree[U]): (List[RoseTree[(T, U)]], Option[E], Randomizer) = {
    /*val (shrunkRtOfT, errOpt1, rnd2) = depthFirstShrinks(value => fun(value, roseTreeOfU.value), rnd)
    val bestT = shrunkRtOfT.headOption.getOrElse(this)
    val bestTValue = bestT.value
    val (shrunkRtOfU, errOpt2, rnd3) = roseTreeOfU.depthFirstShrinks(value => fun(bestTValue, value), rnd2)
    val bestU = shrunkRtOfU.headOption.getOrElse(roseTreeOfU)
    val bestUValue = bestU.value
    val errOpt = List(errOpt1, errOpt2).flatten.lastOption
    (List(bestT.map(t => (t, bestUValue))), errOpt, rnd3)*/
    val rtOfTU: RoseTree[(T, U)] = 
      map { v =>
        (v, roseTreeOfU.value)
      }
    rtOfTU.depthFirstShrinks(v => fun(v._1, v._2), rnd)
  }

  def combineFirstDepthShrinksForFuture[E, U](fun: (T, U) => Future[(Boolean, Option[E])], rnd: Randomizer, roseTreeOfU: RoseTree[U])(implicit execContext: ExecutionContext): Future[(List[RoseTree[(T, U)]], Option[E], Randomizer)] = 
    for {
      (shrunkRtOfT, errOpt1, rnd2) <- depthFirstShrinksForFuture(value => fun(value, roseTreeOfU.value), rnd)
      bestT = shrunkRtOfT.headOption.getOrElse(this)
      bestTValue = bestT.value
      (shrunkRtOfU, errOpt2, rnd3) <- roseTreeOfU.depthFirstShrinksForFuture(value => fun(bestTValue, value), rnd2)
      bestU = shrunkRtOfU.headOption.getOrElse(roseTreeOfU)
      bestUValue = bestU.value
      errOpt = List(errOpt1, errOpt2).flatten.lastOption
    } yield (List(bestT.map(t => (t, bestUValue))), errOpt, rnd3)

  // This makes sense to me say Char is on the inside, then T is Char, and U is (Char, Int). So
  // for each shrunken Char, we'll get the one (Char, Int).
  def map[U](f: T => U): RoseTree[U] = {

    val u: U = f(value) // (Char, Int) the Int is constant, essentially, captured by the T => U function. The T, the Char, is what varies.

    def roseTreeOfTToRoseTreeOfUFun(roseTreeOfT: RoseTree[T]): RoseTree[U] = roseTreeOfT.map(f) 

    def uToListOfRoseTreeOfUFun(u: U, rnd: Randomizer): (List[RoseTree[U]], Randomizer) = {
      val (roseTrees, rnd2) = shrinks(rnd)
      (roseTrees.map(roseTreeOfTToRoseTreeOfUFun), rnd2)
    }

    new RoseTree[U] {
      val value: U = f(thisRoseTreeOfT.value)
      def shrinks(rnd: Randomizer): (List[RoseTree[U]], Randomizer) = {
        def roseTreeOfTToRoseTreeOfUFun(roseTreeOfT: RoseTree[T]): RoseTree[U] = roseTreeOfT.map(f)
        val (roseTrees, rnd2) = thisRoseTreeOfT.shrinks(rnd)
        (roseTrees.map(roseTreeOfTToRoseTreeOfUFun), rnd2)
      }
      override def depthFirstShrinks[E](fun: U => (Boolean, Option[E]), rnd: Randomizer): (List[RoseTree[U]], Option[E], Randomizer) = {
        val (shrunkRtOfT, errOpt1, rnd2) = thisRoseTreeOfT.depthFirstShrinks(value => fun(f(value)), rnd)
        val bestT = shrunkRtOfT.headOption.getOrElse(thisRoseTreeOfT)
        val bestTValue = bestT.value
        //val (shrunkRtOfU, errOpt2, rnd3) = roseTreeOfU.depthFirstShrinks(value => fun(bestTValue, value), rnd2)
        ???
      }
    }
  }

  // So here, we need to go through each of the Ints. U here is Char? No, U is (Char, Int) again? Yes.
  // Ah, and T is Int.
  def flatMap[U](f: T => RoseTree[U]): RoseTree[U] = {

    val roseTreeOfU: RoseTree[U] = f(value) // One RoseTree[(Char, Int)]

    val u: U = roseTreeOfU.value // One (Char, Int)

    def roseTreeOfTs(rnd: Randomizer): (List[RoseTree[T]], Randomizer) = {
      val (roseTrees, rnd2) = shrinks(rnd)
      (Rose(value) :: roseTrees, rnd2)
    } // List of RoseTree[Int]
    // Can I add to this a RoseTree(value, emptyListFun)?

    // Ah, I'm not using value, which is T, except to get the roseTreeOfU oh and the List[RoseTree[T]]
    // That's the one that's missing. I need to add one more at the top, which is value (: T)...

    def roseTreeOfTToRoseTreeOfUFun(roseTreeOfT: RoseTree[T]): RoseTree[U] = f(roseTreeOfT.value)

    def uToListOfRoseTreeOfUFun(u: U, rnd: Randomizer): (List[RoseTree[U]], Randomizer) = {
      val (roseTrees, rnd2) = roseTreeOfTs(rnd)
      (roseTrees.map(roseTreeOfTToRoseTreeOfUFun), rnd2)
    }

    // So yes, I get one u (one (Char, Int)), which is the root of the tree. I now need to make the
    // tree part. It should use the same Char but go through the Ints.
    //RoseTree(u, uToListOfRoseTreeOfUFun)
    new RoseTree[U] {

      val value: U = {
        val roseTreeOfU: RoseTree[U] = f(thisRoseTreeOfT.value)
        roseTreeOfU.value
      }

      def shrinks(rnd: Randomizer): (List[RoseTree[U]], Randomizer) = {
        def roseTreeOfTs(rnd: Randomizer): (List[RoseTree[T]], Randomizer) = {
          val (roseTrees, rnd2) = thisRoseTreeOfT.shrinks(rnd)
          (Rose(thisRoseTreeOfT.value) :: roseTrees, rnd2)
        } // List of RoseTree[Int]

        def roseTreeOfTToRoseTreeOfUFun(roseTreeOfT: RoseTree[T]): RoseTree[U] = f(roseTreeOfT.value)

        val (roseTrees, rnd2) = roseTreeOfTs(rnd)
        (roseTrees.map(roseTreeOfTToRoseTreeOfUFun), rnd2)
      }
    }
    // CharIntPair => roseTreeOfInts.map(roseTreeOfInt => f(roseTreeOfInt.value))
    // So I think right there it should be working. But I am throwing away 
  }

  override def toString: String = s"RoseTree($value)"
}

// Terminal node of a RoseTree is a Rose.
case class Rose[T](value: T) extends RoseTree[T] {
  def shrinks(rnd: Randomizer): (List[RoseTree[T]], Randomizer) = (List.empty, rnd)
  override def toString: String = s"Rose($value)"
}


/*
import org.scalatest.prop._
def unfold[a](rt: RoseTree[a], indent: String = ""): Unit = {
  println(s"$indent ${rt.value}")
  val (roseTrees, rnd2) = rt.shrinks(Randomizer.default)
  roseTrees.foreach(t => unfold(t, s"$indent  "))
}

case class RoseBush[T](o: T, shr: (T, Randomizer) => (List[RoseTree[T]], Randomizer)) extends RoseTree[T] {
  val value: T = o
  def shrinks(rnd: Randomizer): (List[RoseTree[T]], Randomizer) = shr(o, rnd)
}

def intShr: (Int, Randomizer) => (List[RoseTree[Int]], Randomizer) = { (n: Int, rnd: Randomizer) =>
  val roseTrees = if (n > 0) (0 to n - 1).toList.reverse.map(x => RoseBush(x, intShr)) else List.empty
  (roseTrees, rnd)
}

def dubShr: (Double, Randomizer) => (List[RoseTree[Double]], Randomizer) = { (n: Double, rnd: Randomizer) =>
  val roseTrees = if (n > 0) (0 to n.toInt - 1).toList.map(_.toDouble).reverse.map(x => RoseBush(x, dubShr)) else List.empty
  (roseTrees, rnd)
}

def charShr: (Char, Randomizer) => (List[RoseTree[Char]], Randomizer) = { (c: Char, rnd: Randomizer) =>
  val roseTrees = if (c.toLower > 'a') ('a' to (c - 1).toChar).toList.reverse.map(x => RoseBush(x, charShr)) else List.empty
  (roseTrees, rnd)
}

https://www.well-typed.com/blog/2019/05/integrated-shrinking/

scala> for {
     |   i <- RoseTree(2, intShr)
     |   c <- RoseTree('c', charShr)
     | } yield (c, i)
res5: org.scalatest.prop.RoseTree[(Char, Int)] = RoseTree((c,2),org.scalatest.prop.RoseTree$$Lambda$12440/1544455474@1a80e1d9)

scala> unfold(res5)
 (c,2)
   (c,2)
     (b,2)
       (a,2)
     (a,2)
   (c,1)
     (b,1)
       (a,1)
     (a,1)
   (c,0)
     (b,0)
       (a,0)
     (a,0)
*/


