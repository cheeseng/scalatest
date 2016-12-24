/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalactic.anyvals

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions

/**
 * An <code>AnyVal</code> for positive <code>Double</code>s.
 *
 * <p>
 * Note: a <code>PosDouble</code> may not equal 0. If you want positive
 * number or 0, use [[PosZDouble]].
 * A <code>PosDouble</code> may have value <code>Double.PositiveInfinity</code>,
 * but cannot have value <code>Double.NegativeInfinity</code> or <code>Double.NaN</code>.
 * </p>
 *
 * <p>
 * Because <code>PosDouble</code> is an <code>AnyVal</code> it
 * will usually be as efficient as an <code>Double</code>, being
 * boxed only when a <code>Double</code> would have been boxed.
 * </p>
 * 
 * <p>
 * The <code>PosDouble.apply</code> factory method is
 * implemented in terms of a macro that checks literals for
 * validity at compile time. Calling
 * <code>PosDouble.apply</code> with a literal
 * <code>Double</code> value will either produce a valid
 * <code>PosDouble</code> instance at run time or an error at
 * compile time. Here's an example:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; import anyvals._
 * import anyvals._
 *
 * scala&gt; PosDouble(1.0)
 * res1: org.scalactic.anyvals.PosDouble = PosDouble(1.0)
 *
 * scala&gt; PosDouble(0.0)
 * &lt;console&gt;:14: error: PosDouble.apply can only be invoked on a positive (i &gt; 0.0) floating point literal, like PosDouble(42.0).
 *               PosDouble(0.0)
 *                        ^
 * </pre>
 *
 * <p>
 * <code>PosDouble.apply</code> cannot be used if the value
 * being passed is a variable (<em>i.e.</em>, not a literal),
 * because the macro cannot determine the validity of variables
 * at compile time (just literals). If you try to pass a
 * variable to <code>PosDouble.apply</code>, you'll get a
 * compiler error that suggests you use a different factor
 * method, <code>PosDouble.from</code>, instead:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val x = 1.0
 * x: Double = 1.0
 *
 * scala&gt; PosDouble(x)
 * &lt;console&gt;:15: error: PosDouble.apply can only be invoked on a floating point literal, like PosDouble(42.0). Please use PosDouble.from instead.
 *               PosDouble(x)
 *                        ^
 * </pre>
 *
 * <p>
 * The <code>PosDouble.from</code> factory method will inspect
 * the value at runtime and return an
 * <code>Option[PosDouble]</code>. If the value is valid,
 * <code>PosDouble.from</code> will return a
 * <code>Some[PosDouble]</code>, else it will return a
 * <code>None</code>.  Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; PosDouble.from(x)
 * res4: Option[org.scalactic.anyvals.PosDouble] = Some(PosDouble(1.0))
 *
 * scala&gt; val y = 0.0
 * y: Double = 0.0
 *
 * scala&gt; PosDouble.from(y)
 * res5: Option[org.scalactic.anyvals.PosDouble] = None
 * </pre>
 * 
 * <p>
 * The <code>PosDouble.apply</code> factory method is marked
 * implicit, so that you can pass literal <code>Double</code>s
 * into methods that require <code>PosDouble</code>, and get the
 * same compile-time checking you get when calling
 * <code>PosDouble.apply</code> explicitly. Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; def invert(pos: PosDouble): Double = Double.MaxValue - pos
 * invert: (pos: org.scalactic.anyvals.PosDouble)Double
 *
 * scala&gt; invert(1.1)
 * res6: Double = 1.7976931348623157E308
 *
 * scala&gt; invert(Double.MaxValue)
 * res8: Double = 0.0
 *
 * scala&gt; invert(0.0)
 * &lt;console&gt;:15: error: PosDouble.apply can only be invoked on a positive (i &gt; 0.0) floating point literal, like PosDouble(42.0).
 *               invert(0.0)
 *                      ^
 *
 * scala&gt; invert(-1.0)
 * &lt;console&gt;:15: error: PosDouble.apply can only be invoked on a positive (i &gt; 0.0) floating point literal, like PosDouble(42.0).
 *               invert(-1.0)
 *                       ^
 *
 * </pre>
 *
 * <p>
 * This example also demonstrates that the
 * <code>PosDouble</code> companion object also defines implicit
 * widening conversions when a similar conversion is provided in
 * Scala. This makes it convenient to use a
 * <code>PosDouble</code> where a <code>Double</code> is
 * needed. An example is the subtraction in the body of the
 * <code>invert</code> method defined above,
 * <code>Double.MaxValue - pos</code>. Although
 * <code>Double.MaxValue</code> is a <code>Double</code>, which
 * has no <code>-</code> method that takes a
 * <code>PosDouble</code> (the type of <code>pos</code>), you
 * can still subtract <code>pos</code>, because the
 * <code>PosDouble</code> will be implicitly widened to
 * <code>Double</code>.
 * </p>
 *
 * @param value The <code>Double</code> value underlying this <code>PosDouble</code>.
 */ 
final class PosDouble private (val value: Double) extends AnyVal {

  /**
   * A string representation of this <code>PosDouble</code>.
   */
  override def toString: String = s"PosDouble($value)"

  /**
   * Converts this <code>PosDouble</code> to a <code>Byte</code>.
   */
  def toByte: Byte = value.toByte

  /**
   * Converts this <code>PosDouble</code> to a <code>Short</code>.
   */
  def toShort: Short = value.toShort

  /**
   * Converts this <code>PosDouble</code> to a <code>Char</code>.
   */
  def toChar: Char = value.toChar

  /**
   * Converts this <code>PosDouble</code> to an <code>Int</code>.
   */
  def toInt: Int = value.toInt

  /**
   * Converts this <code>PosDouble</code> to a <code>Long</code>.
   */
  def toLong: Long = value.toLong

  /**
   * Converts this <code>PosDouble</code> to a <code>Float</code>.
   */
  def toFloat: Float = value.toFloat

  /**
   * Converts this <code>PosDouble</code> to a <code>Double</code>.
   */
  def toDouble: Double = value.toDouble

  /** Returns this value, unmodified. */
  def unary_+ : PosDouble = this
  /** Returns the negation of this value. */
  def unary_- : Double = -value

  /**
   * Converts this <code>PosDouble</code>'s value to a string then concatenates the given string.
   */
  def +(x: String): String = value + x

  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Byte): Boolean = value < x
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Short): Boolean = value < x
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Char): Boolean = value < x
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Int): Boolean = value < x
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Long): Boolean = value < x
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Float): Boolean = value < x
  /** Returns `true` if this value is less than x, `false` otherwise. */
  def <(x: Double): Boolean = value < x

  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Byte): Boolean = value <= x
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Short): Boolean = value <= x
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Char): Boolean = value <= x
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Int): Boolean = value <= x
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Long): Boolean = value <= x
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Float): Boolean = value <= x
  /** Returns `true` if this value is less than or equal to x, `false` otherwise. */
  def <=(x: Double): Boolean = value <= x

  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Byte): Boolean = value > x
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Short): Boolean = value > x
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Char): Boolean = value > x
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Int): Boolean = value > x
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Long): Boolean = value > x
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Float): Boolean = value > x
  /** Returns `true` if this value is greater than x, `false` otherwise. */
  def >(x: Double): Boolean = value > x

  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Byte): Boolean = value >= x
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Short): Boolean = value >= x
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Char): Boolean = value >= x
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Int): Boolean = value >= x
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Long): Boolean = value >= x
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Float): Boolean = value >= x
  /** Returns `true` if this value is greater than or equal to x, `false` otherwise. */
  def >=(x: Double): Boolean = value >= x

  /** Returns the sum of this value and `x`. */
  def +(x: Byte): Double = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Short): Double = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Char): Double = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Int): Double = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Long): Double = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Float): Double = value + x
  /** Returns the sum of this value and `x`. */
  def +(x: Double): Double = value + x

  /**
   * Returns the <code>PosDouble</code> sum of this <code>PosDouble</code>'s value and the given <code>PosZDouble</code> value.
   *
   * <p>
   * This method will always succeed (not throw an exception) because
   * adding a positive Double and zero or a positive Double and another
   * positive Double will always result in another positive Double
   * value (though the result may be positive infinity).
   * </p>
   */
  def plus(x: PosZDouble): PosDouble = PosDouble.ensuringValid(value + x.value)

  /** Returns the difference of this value and `x`. */
  def -(x: Byte): Double = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Short): Double = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Char): Double = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Int): Double = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Long): Double = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Float): Double = value - x
  /** Returns the difference of this value and `x`. */
  def -(x: Double): Double = value - x

  /** Returns the product of this value and `x`. */
  def *(x: Byte): Double = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Short): Double = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Char): Double = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Int): Double = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Long): Double = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Float): Double = value * x
  /** Returns the product of this value and `x`. */
  def *(x: Double): Double = value * x

  /** Returns the quotient of this value and `x`. */
  def /(x: Byte): Double = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Short): Double = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Char): Double = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Int): Double = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Long): Double = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Float): Double = value / x
  /** Returns the quotient of this value and `x`. */
  def /(x: Double): Double = value / x

  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Byte): Double = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Short): Double = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Char): Double = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Int): Double = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Long): Double = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Float): Double = value % x
  /** Returns the remainder of the division of this value by `x`. */
  def %(x: Double): Double = value % x

  // TODO: Need Scaladoc
  // Stuff from RichDouble
  def isPosInfinity: Boolean = Double.PositiveInfinity == value

  /**
  * Returns <code>this</code> if <code>this &gt; that</code> or <code>that</code> otherwise.
  */
  def max(that: PosDouble): PosDouble = if (math.max(value, that.value) == value) this else that

  /**
  * Returns <code>this</code> if <code>this &lt; that</code> or <code>that</code> otherwise.
  */
  def min(that: PosDouble): PosDouble = if (math.min(value, that.value) == value) this else that

  // TODO: Need Scaladoc
  def isWhole = {
    val longValue = value.toLong
    longValue.toDouble == value || longValue == Long.MaxValue && value < Double.PositiveInfinity || longValue == Long.MinValue && value > Double.NegativeInfinity
  }

  // TODO: Scaladoc
  def round: PosZLong = PosZLong.ensuringValid(math.round(value)) // Also could be zero.
  def ceil: PosDouble = PosDouble.ensuringValid(math.ceil(value)) // I think this one is safe, but try NaN
  def floor: PosZDouble = PosZDouble.ensuringValid(math.floor(value)) // Could be zero.

  /** Converts an angle measured in degrees to an approximately equivalent
  * angle measured in radians.
  *
  * @return the measurement of the angle x in radians.
  */
  def toRadians: Double = math.toRadians(value)

  /** Converts an angle measured in radians to an approximately equivalent
  * angle measured in degrees.
  * @return the measurement of the angle x in degrees.
  */
  def toDegrees: Double = math.toDegrees(value)

  // adapted from RichInt:
  /**
  * Create a <code>Range</code> from this <code>PosDouble</code> value
  * until the specified <code>end</code> (exclusive) with step value 1.
  *
  * @param end The final bound of the range to make.
  * @return A [[scala.collection.immutable.Range.Partial[Double, NumericRange[Double]]]] from `this` up to but
  * not including `end`.
  */
  def until(end: Double): Range.Partial[Double, NumericRange[Double]] =
    value.until(end)

  /**
  * Create a <code>Range</code> from this <code>PosDouble</code> value
  * until the specified <code>end</code> (exclusive) with the specified <code>step</code> value.
  *
  * @param end The final bound of the range to make.
  * @param end The final bound of the range to make.
  * @param step The number to increase by for each step of the range.
  * @return A [[scala.collection.immutable.NumericRange.Exclusive[Double]]] from `this` up to but
  * not including `end`.
  */
  def until(end: Double, step: Double): NumericRange.Exclusive[Double] =
    value.until(end, step)

  /**
  * Create an inclusive <code>Range</code> from this <code>PosDouble</code> value
  * to the specified <code>end</code> with step value 1.
  *
  * @param end The final bound of the range to make.
  * @return A [[scala.collection.immutable.Range.Partial[Double, NumericRange[Double]]]] from `'''this'''` up to
  * and including `end`.
  */
  def to(end: Double): Range.Partial[Double, NumericRange[Double]] =
    value.to(end)

  /**
  * Create an inclusive <code>Range</code> from this <code>PosDouble</code> value
  * to the specified <code>end</code> with the specified <code>step</code> value.
  *
  * @param end The final bound of the range to make.
  * @param step The number to increase by for each step of the range.
  * @return A [[scala.collection.immutable.NumericRange.Inclusive[Double]]] from `'''this'''` up to
  * and including `end`.
  */
  def to(end: Double, step: Double): NumericRange.Inclusive[Double] =
    value.to(end, step)

  /**
   * Applies the passed <code>Double =&gt; Double</code> function to the underlying <code>Double</code>
   * value, and if the result is positive, returns the result wrapped in a <code>PosDouble</code>,
   * else throws <code>AssertionError</code>.
   *
   * <p>
   * This method will inspect the result of applying the given function to this
   * <code>PosDouble</code>'s underlying <code>Double</code> value and if the result
   * is greater than <code>0.0</code>, it will return a <code>PosDouble</code> representing that value.
   * Otherwise, the <code>Double</code> value returned by the given function is
   * <code>0.0</code> or negative, so this method will throw <code>AssertionError</code>.
   * </p>
   *
   * <p>
   * This method differs from a vanilla <code>assert</code> or <code>ensuring</code>
   * call in that you get something you didn't already have if the assertion
   * succeeds: a <em>type</em> that promises an <code>Double</code> is positive. 
   * With this method, you are asserting that you are convinced the result of
   * the computation represented by applying the given function to this <code>PosDouble</code>'s
   * value will not produce zero, a negative number, including <code>Double.NegativeInfinity</code>, or <code>Double.NaN</code>.
   * Instead of producing such invalid values, this method will throw <code>AssertionError</code>.
   * </p>
   *
   * @param f the <code>Double =&gt; Double</code> function to apply to this <code>PosDouble</code>'s
   *     underlying <code>Double</code> value.
   * @return the result of applying this <code>PosDouble</code>'s underlying <code>Double</code> value to
   *     to the passed function, wrapped in a <code>PosDouble</code> if it is positive (else throws <code>AssertionError</code>).
   * @throws AssertionError if the result of applying this <code>PosDouble</code>'s underlying <code>Double</code> value to
   *     to the passed function is not positive.
   */
  def ensuringValid(f: Double => Double): PosDouble = {
    val candidateResult: Double = f(value)
    if (PosDoubleMacro.isValid(candidateResult)) new PosDouble(candidateResult)
    else throw new AssertionError(s"$candidateResult, the result of applying the passed function to $value, was not a valid PosDouble")
  }
}

/**
 * The companion object for <code>PosDouble</code> that offers
 * factory methods that produce <code>PosDouble</code>s,
 * implicit widening conversions from <code>PosDouble</code> to
 * other numeric types, and maximum and minimum constant values
 * for <code>PosDouble</code>.
 */
object PosDouble {
  /**
   * The largest value representable as a positive <code>Double</code>,
   * which is <code>PosDouble(1.7976931348623157E308)</code>.
   */
  final val MaxValue: PosDouble = PosDouble.ensuringValid(Double.MaxValue)

  /**
   * The smallest value representable as a positive
   * <code>Double</code>, which is <code>PosDouble(4.9E-324)</code>.
   */
  final val MinValue: PosDouble = PosDouble.ensuringValid(Double.MinPositiveValue) // Can't use the macro here

  /**
   * The smallest value representable as a positive
   * <code>Double</code>, which is <code>PosDouble(4.9E-324)</code>.
   *
   * <p>
   * Note: This returns the same value as <code>PosDouble.MinValue</code>.
   * </p>
   */
  final val MinPositiveValue: PosDouble = PosDouble.ensuringValid(Double.MinPositiveValue) // Can't use the macro here

  /**
   * The positive infinity value, which is <code>PosDouble.ensuringValid(Double.PositiveInfinity)</code>.
   */
  final val PositiveInfinity: PosDouble = PosDouble.ensuringValid(Double.PositiveInfinity) // Can't use the macro here

  /**
   * A factory method that produces an <code>Option[PosDouble]</code> given a
   * <code>Double</code> value.
   *
   * <p>
   * This method will inspect the passed <code>Double</code> value and if
   * it is a positive <code>Double</code>, <em>i.e.</em>, a value greater
   * than 0.0, it will return a <code>PosDouble</code> representing that value,
   * wrapped in a <code>Some</code>. Otherwise, the passed <code>Double</code>
   * value is 0.0 or negative, so this method will return <code>None</code>.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code>
   * factory method in that <code>apply</code> is implemented
   * via a macro that inspects <code>Double</code> literals at
   * compile time, whereas <code>from</code> inspects
   * <code>Double</code> values at run time.
   * </p>
   *
   * @param value the <code>Double</code> to inspect, and if positive, return
   *     wrapped in a <code>Some[PosDouble]</code>.
   * @return the specified <code>Double</code> value wrapped in a
   *     <code>Some[PosDouble]</code>, if it is positive, else
   *     <code>None</code>.
   */
  def from(value: Double): Option[PosDouble] =
    if (PosDoubleMacro.isValid(value)) Some(new PosDouble(value)) else None

  /**
   * A factory/assertion method that produces a <code>PosDouble</code> given a
   * valid <code>Double</code> value, or throws <code>AssertionError</code>,
   * if given an invalid <code>Double</code> value.
   *
   * <p>
   * This method will inspect the passed <code>Double</code> value and if
   * it is a positive <code>Double</code>, <em>i.e.</em>, a value greater
   * than 0.0, it will return a <code>PosDouble</code> representing that value.
   * Otherwise, the passed <code>Double</code> value is 0.0 or negative, so this
   * method will throw <code>AssertionError</code>.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code>
   * factory method in that <code>apply</code> is implemented
   * via a macro that inspects <code>Double</code> literals at
   * compile time, whereas <code>from</code> inspects
   * <code>Double</code> values at run time.
   * It differs from a vanilla <code>assert</code> or <code>ensuring</code>
   * call in that you get something you didn't already have if the assertion
   * succeeds: a <em>type</em> that promises a <code>Double</code> is positive.
   * </p>
   *
   * @param value the <code>Double</code> to inspect, and if positive, return
   *     wrapped in a <code>PosDouble</code>.
   * @return the specified <code>Double</code> value wrapped in a
   *     <code>PosDouble</code>, if it is positive, else
   *     throws <code>AssertionError</code>.
   * @throws AssertionError if the passed value is not positive
   */
  def ensuringValid(value: Double): PosDouble =
    if (PosDoubleMacro.isValid(value)) new PosDouble(value) else {
      throw new AssertionError(s"$value was not a valid PosDouble")
    }

  /**
   * A predicate method that returns true if a given 
   * <code>Double</code> value is positive.
   *
   * @param value the <code>Double</code> to inspect, and if positive, return true.
   * @return true if the specified <code>Double</code> is positive, else false.
   */
  def isValid(value: Double): Boolean = PosDoubleMacro.isValid(value)

  /**
   * A factory method that produces a <code>PosDouble</code> given a
   * <code>Double</code> value and a default <code>PosDouble</code>.
   *
   * <p>
   * This method will inspect the passed <code>Double</code> value and if
   * it is a positive <code>Double</code>, <em>i.e.</em>, a value greater
   * than 0.0, it will return a <code>PosDouble</code> representing that value.
   * Otherwise, the passed <code>Double</code> value is 0.0 or negative, so this
   * method will return the passed <code>default</code> value.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code>
   * factory method in that <code>apply</code> is implemented
   * via a macro that inspects <code>Double</code> literals at
   * compile time, whereas <code>from</code> inspects
   * <code>Double</code> values at run time.
   * </p>
   *
   * @param value the <code>Double</code> to inspect, and if positive, return.
   * @param default the <code>PosDouble</code> to return if the passed
   *     <code>Double</code> value is not positive.
   * @return the specified <code>Double</code> value wrapped in a
   *     <code>PosDouble</code>, if it is positive, else the
   *     <code>default</code> <code>PosDouble</code> value.
   */
  def fromOrElse(value: Double, default: => PosDouble): PosDouble =
    if (PosDoubleMacro.isValid(value)) new PosDouble(value) else default

  import language.experimental.macros
  import scala.language.implicitConversions

  /**
   * A factory method, implemented via a macro, that produces a
   * <code>PosDouble</code> if passed a valid <code>Double</code>
   * literal, otherwise a compile time error.
   *
   * <p>
   * The macro that implements this method will inspect the
   * specified <code>Double</code> expression at compile time. If
   * the expression is a positive <code>Double</code> literal,
   * <em>i.e.</em>, with a value greater than 0.0, it will return
   * a <code>PosDouble</code> representing that value.  Otherwise,
   * the passed <code>Double</code> expression is either a literal
   * that is 0.0 or negative, or is not a literal, so this method
   * will give a compiler error.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>from</code>
   * factory method in that this method is implemented via a
   * macro that inspects <code>Double</code> literals at compile
   * time, whereas <code>from</code> inspects <code>Double</code>
   * values at run time.
   * </p>
   *
   * @param value the <code>Double</code> literal expression to
   *     inspect at compile time, and if positive, to return
   *     wrapped in a <code>PosDouble</code> at run time.
   * @return the specified, valid <code>Double</code> literal
   *     value wrapped in a <code>PosDouble</code>. (If the
   *     specified expression is not a valid <code>Double</code>
   *     literal, the invocation of this method will not
   *     compile.)
   */
  implicit def apply(value: Double): PosDouble = macro PosDoubleMacro.apply

  /**
   * Implicit widening conversion from <code>PosDouble</code> to
   * <code>Double</code>.
   *
   * @param pos the <code>PosDouble</code> to widen
   * @return the <code>Double</code> value underlying the specified
   *     <code>PosDouble</code>
   */
  implicit def widenToDouble(pos: PosDouble): Double = pos.value

  /**
   * Implicit widening conversion from <code>PosDouble</code> to
   * <code>PosZDouble</code>.
   *
   * @param pos the <code>PosDouble</code> to widen
   * @return the <code>Double</code> value underlying the specified
   *     <code>PosDouble</code> wrapped in a <code>PosZDouble</code>.
   */
  implicit def widenToPosZDouble(pos: PosDouble): PosZDouble = PosZDouble.ensuringValid(pos.value)

  /**
    * Implicit widening conversion from <code>PosDouble</code> to
    * <code>NonZeroDouble</code>.
    *
    * @param pos the <code>PosDouble</code> to widen
    * @return the <code>Double</code> value underlying the specified
    *     <code>PosDouble</code> wrapped in a <code>NonZeroDouble</code>.
    */
  implicit def widenToNonZeroDouble(pos: PosDouble): NonZeroDouble = NonZeroDouble.ensuringValid(pos.value)

  /**
   * Returns the <code>PosDouble</code> sum of the passed <code>PosDouble</code> value `x` and <code>PosZDouble</code> value `y`.
   *
   * <p>
   * This method will always succeed (not throw an exception) because
   * adding a positive Double and zero or a positive Double and another
   * positive Double will always result in another positive Double
   * value (though the result may be positive infinity).
   * </p>
   *
   * <p>
   * This overloaded form of the method is used when there are just two arguments so that
   * boxing is avoided. The overloaded <code>sumOf</code> that takes a varargs of
   * <code>PosZDouble</code> starting at the third parameter can sum more than two
   * values, but will entail boxing and may therefore be less efficient.
   * </p>
   */
  def sumOf(x: PosDouble, y: PosZDouble): PosDouble = PosDouble.ensuringValid(x.value + y.value)

  /**
   * Returns the <code>PosDouble</code> sum of the passed <code>PosDouble</code> value `first`, the <code>PosZDouble</code>
   * value `second`, and the <code>PosDouble</code> values passed as varargs `rest`.
   *
   * <p>
   * This method will always succeed (not throw an exception) because
   * adding a positive Double and one or more zeros or positive Doubles
   * will always result in another positive Double
   * value (though the result may be positive infinity).
   * </p>
   *
   * <p>
   * This overloaded form of the <code>sumOf</code> method can sum more than two
   * values, but unlike its two-arg sibling, will entail boxing.
   * </p>
   */
  def sumOf(first: PosDouble, second: PosZDouble, rest: PosZDouble*): PosDouble =
    PosDouble.ensuringValid(first.value + second.value + rest.map(_.value).sum)

  /**
   * Implicit Ordering instance.
   */
  implicit val posDoubleOrd: Ordering[PosDouble] =
    new Ordering[PosDouble] {
      def compare(x: PosDouble, y: PosDouble): Int = x.toDouble.compare(y)
    } 
}

