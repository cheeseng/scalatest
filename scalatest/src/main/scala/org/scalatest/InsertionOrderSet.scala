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
package org.scalatest

private[scalatest] class InsertionOrderSet[A](elements: scala.collection.Seq[A]) extends scala.collection.Set[A] {

  val list = elements.distinct

  def contains(key: A): Boolean = list.contains(key)
  def iterator: Iterator[A] = list.iterator
  override def +(elem: A) = InsertionOrderSet[A](list :+ elem)
  override def -(elem: A) =
    InsertionOrderSet[A](list.filter(_ != elem))
  def diff(that: scala.collection.Set[A]): InsertionOrderSet[A] = InsertionOrderSet(elements.diff(that.toSeq))
}

private[scalatest] object InsertionOrderSet {
  def apply[A](elements: scala.collection.Seq[A]): InsertionOrderSet[A] = new InsertionOrderSet(elements)
}
