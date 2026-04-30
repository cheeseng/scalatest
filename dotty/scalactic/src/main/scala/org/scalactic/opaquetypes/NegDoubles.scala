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
package org.scalactic.opaquetypes

import org.scalactic.Resources
import scala.compiletime.{ constValueOpt, error }
import scala.util.{Try, Success, Failure}
import org.scalactic.{Validation, Pass, Fail}
import org.scalactic.{Or, Good, Bad}

object NegDoubles {
  opaque type NegZDouble = Double
  object NegZDouble {
    def ensuringValid(d: Double): NegZDouble = 
      if (d > 0) 
        throw new AssertionError(Resources.invalidNegZDouble)
      else d
  }

  /** Opaque type representing a non-positive, finite <code>Double</code> value.
    *
    * <p>
    * Instances of this type are guaranteed to satisfy both <code>&lt;= 0.0</code>
    * and <code>isFinite</code> (i.e. neither <code>Double.PositiveInfinity</code> nor
    * <code>Double.NegativeInfinity</code> nor <code>Double.NaN</code>).
    * </p>
    *
    * <p>
    * <code>NegZFiniteDouble</code> is a strict subtype of [[NegZDouble]], so anywhere a
    * <code>NegZDouble</code> is accepted a <code>NegZFiniteDouble</code> may be passed
    * without an explicit conversion.
    * </p>
    *
    * <p>
    * Use the compile-time <code>apply</code> overloads to construct instances from
    * literals, or the runtime factory methods [[NegZFiniteDouble.from]],
    * [[NegZFiniteDouble.ensuringValid]], and [[NegZFiniteDouble.fromOrElse]] for
    * values known only at runtime.
    * </p>
    */
  opaque type NegZFiniteDouble <: NegZDouble = Double

  /** Companion object for the [[NegZFiniteDouble]] opaque type.
    *
    * Provides compile-time <code>apply</code> overloads, runtime factory and
    * validation methods, implicit widening conversions, and an extension method
    * for post-computation validation.
    */
  object NegZFiniteDouble {

    /** Implicitly widens a [[NegZFiniteDouble]] to a plain <code>Double</code>. */
    given Conversion[NegZFiniteDouble, Double] with {
      def apply(x: NegZFiniteDouble): Double = x.toDouble
    }

    /** Converts a compile-time non-positive <code>Int</code> literal to a [[NegZFiniteDouble]].
      *
      * The inline overload is checked at compile time; the runtime overload
      * performs no validation (all non-positive <code>Int</code> values widen safely
      * to a non-positive, finite <code>Double</code>).
      *
      * @throws compile-time error if the literal is positive or not a literal
      */
    given Conversion[Int, NegZFiniteDouble] with {
      inline def apply[I <: Int & Singleton](inline x: I): NegZFiniteDouble =
        inline constValueOpt[I] match {
          case Some(v: Int) =>
            inline if v > 0 then
              error("NegZFiniteDouble cannot be instantiated with a positive integer literal")
            else
              v.toDouble.asInstanceOf[NegZFiniteDouble]
          case None =>
            error("NegZFiniteDouble conversion requires an integer literal")
        }
      def apply(x: Int): NegZFiniteDouble = x.toDouble
    }

    /** Converts a compile-time non-positive <code>Long</code> literal to a [[NegZFiniteDouble]].
      *
      * The inline overload is checked at compile time; the runtime overload
      * performs no validation (all non-positive <code>Long</code> values widen
      * safely to a finite <code>Double</code>).
      *
      * @throws compile-time error if the literal is positive or not a literal
      */
    given Conversion[Long, NegZFiniteDouble] with {
      inline def apply[L <: Long & Singleton](inline x: L): NegZFiniteDouble =
        inline constValueOpt[L] match {
          case Some(v: Long) =>
            inline if v > 0L then
              error("NegZFiniteDouble cannot be instantiated with a positive long literal")
            else
              v.toDouble.asInstanceOf[NegZFiniteDouble]
          case None =>
            error("NegZFiniteDouble conversion requires a long literal")
        }
      def apply(x: Long): NegZFiniteDouble = x.toDouble
    }

    /** Converts a compile-time non-positive, finite <code>Float</code> literal to a [[NegZFiniteDouble]].
      *
      * The inline overload is checked at compile time and rejects positive values
      * and negative infinity.  The runtime overload performs no validation.
      *
      * @throws compile-time error if the literal is positive, negative infinity, or not a literal
      */
    given Conversion[Float, NegZFiniteDouble] with {
      inline def apply[F <: Float & Singleton](inline x: F): NegZFiniteDouble =
        inline constValueOpt[F] match {
          case Some(v: Float) =>
            inline if v > 0.0f || v == Float.NegativeInfinity then
              error("NegZFiniteDouble cannot be instantiated with a positive float literal or negative infinity")
            else
              v.toDouble.asInstanceOf[NegZFiniteDouble]
          case None =>
            error("NegZFiniteDouble conversion requires a float literal")
        }
      def apply(x: Float): NegZFiniteDouble = x.toDouble
    }

    /** Converts a compile-time non-positive, finite <code>Double</code> literal to a [[NegZFiniteDouble]].
      *
      * The inline overload is checked at compile time and rejects positive values
      * and negative infinity.  The runtime overload performs no validation.
      *
      * @throws compile-time error if the literal is positive, negative infinity, or not a literal
      */
    given Conversion[Double, NegZFiniteDouble] with {
      inline def apply[D <: Double & Singleton](inline x: D): NegZFiniteDouble =
        inline constValueOpt[D] match {
          case Some(v: Double) =>
            inline if v > 0.0 || v == Double.NegativeInfinity then
              error("NegZFiniteDouble cannot be instantiated with a positive double literal or negative infinity")
            else
              v.asInstanceOf[NegZFiniteDouble]
          case None =>
            error("NegZFiniteDouble conversion requires a double literal")
        }
      def apply(x: Double): NegZFiniteDouble = x
    }

    /** Compile-time factory for creating a [[NegZFiniteDouble]] from a <code>Double</code> literal.
      *
      * This inline method inspects the provided double literal at compile time
      * and rejects positive values, <code>Double.NegativeInfinity</code>, and
      * <code>Double.PositiveInfinity</code>.  Use it as: <code>NegZFiniteDouble(-5.0)</code>.
      * For runtime values use [[ensuringValid]] or [[from]].
      *
      * @tparam D the singleton <code>Double</code> literal type
      * @param d the <code>Double</code> literal
      * @return a [[NegZFiniteDouble]] representing the given non-positive, finite literal
      * @throws compile-time error if the literal is positive, infinite, or not a literal
      */
    inline def apply[D <: Double & Singleton](inline d: D): NegZFiniteDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v > 0.0 || v == Double.NegativeInfinity || v == Double.PositiveInfinity then
            error("NegZFiniteDouble cannot be instantiated with a positive double literal or infinity")
          else
            v.asInstanceOf[NegZFiniteDouble]
        case None =>
          error("NegZFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[NegZFiniteDouble]] from a <code>Float</code> literal.
      *
      * This inline method inspects the provided float literal at compile time
      * and rejects positive values and negative infinity.
      *
      * @tparam F the singleton <code>Float</code> literal type
      * @param f the <code>Float</code> literal
      * @return a [[NegZFiniteDouble]] representing the given non-positive literal widened to <code>Double</code>
      * @throws compile-time error if the literal is positive, negative infinity, or not a literal
      */
    inline def apply[F <: Float & Singleton](inline f: F): NegZFiniteDouble =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v > 0.0f || v == Float.NegativeInfinity then
            error("NegZFiniteDouble cannot be instantiated with a positive float literal or negative infinity")
          else
            v.toDouble.asInstanceOf[NegZFiniteDouble]
        case None =>
          error("NegZFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[NegZFiniteDouble]] from a <code>Long</code> literal.
      *
      * This inline method inspects the provided long literal at compile time
      * and rejects positive values.
      *
      * @tparam L the singleton <code>Long</code> literal type
      * @param l the <code>Long</code> literal
      * @return a [[NegZFiniteDouble]] representing the given non-positive literal widened to <code>Double</code>
      * @throws compile-time error if the literal is positive or not a literal
      */
    inline def apply[L <: Long & Singleton](inline l: L): NegZFiniteDouble =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v > 0L then
            error("NegZFiniteDouble cannot be instantiated with a positive long literal")
          else
            v.toDouble.asInstanceOf[NegZFiniteDouble]
        case None =>
          error("NegZFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[NegZFiniteDouble]] from an <code>Int</code> literal.
      *
      * This inline method inspects the provided integer literal at compile time
      * and rejects positive values.
      *
      * @tparam I the singleton <code>Int</code> literal type
      * @param i the <code>Int</code> literal
      * @return a [[NegZFiniteDouble]] representing the given non-positive literal widened to <code>Double</code>
      * @throws compile-time error if the literal is positive or not a literal
      */
    inline def apply[I <: Int & Singleton](inline i: I): NegZFiniteDouble =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v > 0 then
            error("NegZFiniteDouble cannot be instantiated with a positive integer literal")
          else
            v.toDouble.asInstanceOf[NegZFiniteDouble]
        case None =>
          error("NegZFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Returns <code>true</code> if the provided <code>Double</code> is a valid [[NegZFiniteDouble]]
      * value — that is, if it is both <code>&lt;= 0.0</code> and finite (<code>isFinite</code>).
      *
      * @param value the <code>Double</code> to validate
      * @return <code>true</code> if <code>value &lt;= 0.0 &amp;&amp; value.isFinite</code>, <code>false</code> otherwise
      */
    def isValid(value: Double): Boolean = value <= 0.0 && value.isFinite

    /** Returns <code>Some(NegZFiniteDouble)</code> if the given <code>Double</code> is a valid
      * [[NegZFiniteDouble]] (non-positive and finite), or <code>None</code> otherwise.
      *
      * @param d the <code>Double</code> to inspect
      * @return <code>Some(NegZFiniteDouble)</code> if <code>d &lt;= 0.0</code> and finite, else <code>None</code>
      */
    def from(d: Double): Option[NegZFiniteDouble] =
      if (isValid(d)) Some(d) else None

    /** Returns the given <code>Double</code> as a [[NegZFiniteDouble]] if it is valid,
      * or throws <code>AssertionError</code> if it is not.
      *
      * <p>
      * A value is valid if it is <code>&lt;= 0.0</code> and finite.
      * </p>
      *
      * @param d the <code>Double</code> to return as a [[NegZFiniteDouble]]
      * @return <code>d</code> as a [[NegZFiniteDouble]] if valid
      * @throws AssertionError if <code>d</code> is positive or infinite
      */
    /**
      * Returns the given <code>Double</code> as a [[NegZFiniteDouble]] if it is valid,
      * or throws <code>AssertionError</code> if it is not. This inline overload performs
      * a compile-time check for literals and a runtime check otherwise.
      */
    def ensuringValid(d: Double): NegZFiniteDouble =
      if (NegZFiniteDouble.isValid(d)) d
      else throw new AssertionError(Resources.invalidNegZFiniteDouble)

    /** Returns the given <code>Double</code> as a [[NegZFiniteDouble]] if it is valid,
      * or the given <code>default</code> value otherwise.
      *
      * @param value the <code>Double</code> to inspect
      * @param default the [[NegZFiniteDouble]] to return if <code>value</code> is invalid
      * @return <code>value</code> as a [[NegZFiniteDouble]] if valid, else <code>default</code>
      */
    def fromOrElse(value: Double, default: => NegZFiniteDouble): NegZFiniteDouble =
      if (isValid(value)) value else default

    /** A factory/validation method that produces a <code>NegZFiniteDouble</code> wrapped
      * in a <code>Success</code> if the given <code>Double</code> is valid, or an
      * <code>AssertionError</code> wrapped in a <code>Failure</code> if it is not.
      *
      * <p>
      * A value is valid if it is <code>&lt;= 0.0</code> and finite.
      * </p>
      *
      * @param value the <code>Double</code> to inspect
      * @return <code>Success(NegZFiniteDouble)</code> if valid, else <code>Failure(AssertionError)</code>
      */
    def tryingValid(value: Double): Try[NegZFiniteDouble] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidNegZFiniteDouble))

    /** A validation method that produces a <code>Pass</code> given a valid <code>Double</code>
      * value, or a <code>Fail</code> containing an error value produced by passing the
      * invalid <code>Double</code> to the function <code>f</code>.
      *
      * @tparam E the error type produced by <code>f</code>
      * @param value the <code>Double</code> to validate
      * @param f the function applied to an invalid value to produce an error
      * @return <code>Pass</code> if valid, else <code>Fail(f(value))</code>
      */
    def passOrElse[E](value: Double)(f: Double => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** A factory/validation method that produces a <code>NegZFiniteDouble</code> wrapped
      * in a <code>Good</code> if the given <code>Double</code> is valid, or an error
      * value produced by passing the invalid <code>Double</code> to <code>f</code>
      * wrapped in a <code>Bad</code>.
      *
      * @tparam B the error type produced by <code>f</code>
      * @param value the <code>Double</code> to inspect
      * @param f the function applied to an invalid value to produce an error
      * @return <code>Good(NegZFiniteDouble)</code> if valid, else <code>Bad(f(value))</code>
      */
    def goodOrElse[B](value: Double)(f: Double => B): NegZFiniteDouble Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** A factory/validation method that produces a <code>NegZFiniteDouble</code> wrapped
      * in a <code>Right</code> if the given <code>Double</code> is valid, or an error
      * value produced by passing the invalid <code>Double</code> to <code>f</code>
      * wrapped in a <code>Left</code>.
      *
      * @tparam L the error type produced by <code>f</code>
      * @param value the <code>Double</code> to inspect
      * @param f the function applied to an invalid value to produce an error
      * @return <code>Right(NegZFiniteDouble)</code> if valid, else <code>Left(f(value))</code>
      */
    def rightOrElse[L](value: Double)(f: Double => L): Either[L, NegZFiniteDouble] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** The largest value representable as a [[NegZFiniteDouble]], which is
      * <code>NegZFiniteDouble(0.0)</code>.
      */
    val MaxValue: NegZFiniteDouble = 0.0

    /** The smallest (most negative) value representable as a [[NegZFiniteDouble]], which is
      * <code>NegZFiniteDouble(Double.MinValue)</code>.
      */
    val MinValue: NegZFiniteDouble = Double.MinValue

    extension (p: NegZFiniteDouble) {
      /** Return the underlying Double value. */
      def value: Double = p
      /** Applies the given <code>Double => Double</code> function to the underlying
        * <code>Double</code> value, and returns the result as a [[NegZFiniteDouble]] if
        * it is valid, or throws <code>AssertionError</code> if it is not.
        *
        * <p>
        * A result is valid if it is <code><= 0.0</code> and finite.
        * </p>
        *
        * @param f the <code>Double => Double</code> function to apply
        * @return the result of <code>f(p)</code> as a [[NegZFiniteDouble]] if valid
        * @throws AssertionError if the result of <code>f(p)</code> is positive or infinite
        */
      def ensuringValid(f: Double => Double): NegZFiniteDouble = {
        val candidateResult: Double = f(p)
        if (NegZFiniteDouble.isValid(candidateResult)) NegZFiniteDouble.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${p.toString()}, was not a valid NegZFiniteDouble")
      }
    }
  }

  opaque type NegDouble = Double
  object NegDouble {
    def ensuringValid(d: Double): NegDouble = 
      if (d >= 0) 
        throw new AssertionError(Resources.invalidNegDouble)
      else d
  }

  /** Opaque type representing a strictly negative, finite <code>Double</code> value.
    *
    * <p>
    * Instances of this type are guaranteed to satisfy both <code>&lt; 0.0</code>
    * and <code>isFinite</code> (i.e. neither <code>Double.PositiveInfinity</code> nor
    * <code>Double.NegativeInfinity</code> nor <code>Double.NaN</code>).
    * </p>
    *
    * <p>
    * <code>NegFiniteDouble</code> is a strict subtype of both [[NegZFiniteDouble]] and
    * [[NegZDouble]], so it can be widened to either without an explicit conversion.
    * </p>
    *
    * <p>
    * Use the compile-time <code>apply</code> overloads to construct instances from
    * literals, or the runtime factory methods [[NegFiniteDouble.from]],
    * [[NegFiniteDouble.ensuringValid]], and [[NegFiniteDouble.fromOrElse]] for values
    * known only at runtime.
    * </p>
    */
  opaque type NegFiniteDouble <: NegZFiniteDouble = Double

  /** Companion object for the [[NegFiniteDouble]] opaque type.
    *
    * Provides compile-time <code>apply</code> overloads, runtime factory and
    * validation methods, and an extension method for post-computation validation.
    * No implicit widening conversions from primitive types are provided because
    * zero and positive values must be rejected, making a blanket conversion unsafe.
    */
  object NegFiniteDouble {

    /** Compile-time factory for creating a [[NegFiniteDouble]] from a <code>Double</code> literal.
      *
      * This inline method inspects the provided double literal at compile time
      * and rejects zero, positive values, and infinities.
      * Use it as: <code>NegFiniteDouble(-5.0)</code>.
      * For runtime values use [[ensuringValid]] or [[from]].
      *
      * @tparam D the singleton <code>Double</code> literal type
      * @param d the <code>Double</code> literal
      * @return a [[NegFiniteDouble]] representing the given strictly negative, finite literal
      * @throws compile-time error if the literal is non-negative, infinite, or not a literal
      */
    inline def apply[D <: Double & Singleton](inline d: D): NegFiniteDouble =
      inline constValueOpt[D] match {
        case Some(v: Double) =>
          inline if v >= 0.0 || v == Double.NegativeInfinity || v == Double.PositiveInfinity then
            error("NegFiniteDouble cannot be instantiated with a non-negative double literal or infinity")
          else
            v.asInstanceOf[NegFiniteDouble]
        case None =>
          error("NegFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[NegFiniteDouble]] from a <code>Float</code> literal.
      *
      * This inline method inspects the provided float literal at compile time
      * and rejects zero, positive values, and negative infinity.
      *
      * @tparam F the singleton <code>Float</code> literal type
      * @param f the <code>Float</code> literal
      * @return a [[NegFiniteDouble]] representing the given strictly negative literal widened to <code>Double</code>
      * @throws compile-time error if the literal is non-negative, negative infinity, or not a literal
      */
    inline def apply[F <: Float & Singleton](inline f: F): NegFiniteDouble =
      inline constValueOpt[F] match {
        case Some(v: Float) =>
          inline if v >= 0.0f || v == Float.NegativeInfinity then
            error("NegFiniteDouble cannot be instantiated with a non-negative float literal or negative infinity")
          else
            v.toDouble.asInstanceOf[NegFiniteDouble]
        case None =>
          error("NegFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[NegFiniteDouble]] from a <code>Long</code> literal.
      *
      * This inline method inspects the provided long literal at compile time
      * and rejects zero and positive values.
      *
      * @tparam L the singleton <code>Long</code> literal type
      * @param l the <code>Long</code> literal
      * @return a [[NegFiniteDouble]] representing the given strictly negative literal widened to <code>Double</code>
      * @throws compile-time error if the literal is non-negative or not a literal
      */
    inline def apply[L <: Long & Singleton](inline l: L): NegFiniteDouble =
      inline constValueOpt[L] match {
        case Some(v: Long) =>
          inline if v >= 0L then
            error("NegFiniteDouble cannot be instantiated with a non-negative long literal")
          else
            v.toDouble.asInstanceOf[NegFiniteDouble]
        case None =>
          error("NegFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Compile-time factory for creating a [[NegFiniteDouble]] from an <code>Int</code> literal.
      *
      * This inline method inspects the provided integer literal at compile time
      * and rejects zero and positive values.
      *
      * @tparam I the singleton <code>Int</code> literal type
      * @param i the <code>Int</code> literal
      * @return a [[NegFiniteDouble]] representing the given strictly negative literal widened to <code>Double</code>
      * @throws compile-time error if the literal is non-negative or not a literal
      */
    inline def apply[I <: Int & Singleton](inline i: I): NegFiniteDouble =
      inline constValueOpt[I] match {
        case Some(v: Int) =>
          inline if v >= 0 then
            error("NegFiniteDouble cannot be instantiated with a non-negative integer literal")
          else
            v.toDouble.asInstanceOf[NegFiniteDouble]
        case None =>
          error("NegFiniteDouble.apply requires an integer, long, float or double literal")
      }

    /** Returns <code>true</code> if the provided <code>Double</code> is a valid [[NegFiniteDouble]]
      * value — that is, if it is both <code>&lt; 0.0</code> and finite (<code>isFinite</code>).
      *
      * @param value the <code>Double</code> to validate
      * @return <code>true</code> if <code>value &lt; 0.0 &amp;&amp; value.isFinite</code>, <code>false</code> otherwise
      */
    def isValid(value: Double): Boolean = value < 0.0 && value.isFinite

    /** Returns <code>Some(NegFiniteDouble)</code> if the given <code>Double</code> is a valid
      * [[NegFiniteDouble]] (strictly negative and finite), or <code>None</code> otherwise.
      *
      * @param d the <code>Double</code> to inspect
      * @return <code>Some(NegFiniteDouble)</code> if <code>d &lt; 0.0</code> and finite, else <code>None</code>
      */
    def from(d: Double): Option[NegFiniteDouble] =
      if (isValid(d)) Some(d) else None

    /** Returns the given <code>Double</code> as a [[NegFiniteDouble]] if it is valid,
      * or throws <code>AssertionError</code> if it is not.
      *
      * <p>
      * A value is valid if it is <code>&lt; 0.0</code> and finite.
      * </p>
      *
      * @param d the <code>Double</code> to return as a [[NegFiniteDouble]]
      * @return <code>d</code> as a [[NegFiniteDouble]] if valid
      * @throws AssertionError if <code>d</code> is zero, positive, or infinite
      */
    def ensuringValid(d: Double): NegFiniteDouble =
      if (isValid(d))
        d
      else
        throw new AssertionError(Resources.invalidNegFiniteDouble)

    /** Returns the given <code>Double</code> as a [[NegFiniteDouble]] if it is valid,
      * or the given <code>default</code> value otherwise.
      *
      * @param value the <code>Double</code> to inspect
      * @param default the [[NegFiniteDouble]] to return if <code>value</code> is invalid
      * @return <code>value</code> as a [[NegFiniteDouble]] if valid, else <code>default</code>
      */
    def fromOrElse(value: Double, default: => NegFiniteDouble): NegFiniteDouble =
      if (isValid(value)) value else default

    /** A factory/validation method that produces a <code>NegFiniteDouble</code> wrapped
      * in a <code>Success</code> if the given <code>Double</code> is valid, or an
      * <code>AssertionError</code> wrapped in a <code>Failure</code> if it is not.
      *
      * <p>
      * A value is valid if it is <code>&lt; 0.0</code> and finite.
      * </p>
      *
      * @param value the <code>Double</code> to inspect
      * @return <code>Success(NegFiniteDouble)</code> if valid, else <code>Failure(AssertionError)</code>
      */
    def tryingValid(value: Double): Try[NegFiniteDouble] =
      if (isValid(value))
        Success(value)
      else
        Failure(new AssertionError(Resources.invalidNegFiniteDouble))

    /** A validation method that produces a <code>Pass</code> given a valid <code>Double</code>
      * value, or a <code>Fail</code> containing an error value produced by passing the
      * invalid <code>Double</code> to the function <code>f</code>.
      *
      * @tparam E the error type produced by <code>f</code>
      * @param value the <code>Double</code> to validate
      * @param f the function applied to an invalid value to produce an error
      * @return <code>Pass</code> if valid, else <code>Fail(f(value))</code>
      */
    def passOrElse[E](value: Double)(f: Double => E): Validation[E] =
      if (isValid(value)) Pass else Fail(f(value))

    /** A factory/validation method that produces a <code>NegFiniteDouble</code> wrapped
      * in a <code>Good</code> if the given <code>Double</code> is valid, or an error
      * value produced by passing the invalid <code>Double</code> to <code>f</code>
      * wrapped in a <code>Bad</code>.
      *
      * @tparam B the error type produced by <code>f</code>
      * @param value the <code>Double</code> to inspect
      * @param f the function applied to an invalid value to produce an error
      * @return <code>Good(NegFiniteDouble)</code> if valid, else <code>Bad(f(value))</code>
      */
    def goodOrElse[B](value: Double)(f: Double => B): NegFiniteDouble Or B =
      if (isValid(value)) Good(value) else Bad(f(value))

    /** A factory/validation method that produces a <code>NegFiniteDouble</code> wrapped
      * in a <code>Right</code> if the given <code>Double</code> is valid, or an error
      * value produced by passing the invalid <code>Double</code> to <code>f</code>
      * wrapped in a <code>Left</code>.
      *
      * @tparam L the error type produced by <code>f</code>
      * @param value the <code>Double</code> to inspect
      * @param f the function applied to an invalid value to produce an error
      * @return <code>Right(NegFiniteDouble)</code> if valid, else <code>Left(f(value))</code>
      */
    def rightOrElse[L](value: Double)(f: Double => L): Either[L, NegFiniteDouble] =
      if (isValid(value)) Right(ensuringValid(value)) else Left(f(value))

    /** The largest (least negative) value representable as a [[NegFiniteDouble]], which is
      * <code>NegFiniteDouble(-Double.MinPositiveValue)</code>.
      */
    val MaxValue: NegFiniteDouble = -Double.MinPositiveValue

    /** The smallest (most negative) value representable as a [[NegFiniteDouble]], which is
      * <code>NegFiniteDouble(Double.MinValue)</code>.
      */
    val MinValue: NegFiniteDouble = Double.MinValue

    extension (p: NegFiniteDouble) {
      /** Return the underlying Double value. */
      def value: Double = p
      /** Applies the given <code>Double => Double</code> function to the underlying
        * <code>Double</code> value, and returns the result as a [[NegFiniteDouble]] if
        * it is valid, or throws <code>AssertionError</code> if it is not.
        *
        * <p>
        * A result is valid if it is <code>< 0.0</code> and finite.
        * </p>
        *
        * @param f the <code>Double => Double</code> function to apply
        * @return the result of <code>f(p)</code> as a [[NegFiniteDouble]] if valid
        * @throws AssertionError if the result of <code>f(p)</code> is zero, positive, or infinite
        */
      def ensuringValid(f: Double => Double): NegFiniteDouble = {
        val candidateResult: Double = f(p)
        if (NegFiniteDouble.isValid(candidateResult)) NegFiniteDouble.ensuringValid(candidateResult)
        else throw new AssertionError(s"${candidateResult.toString()}, the result of applying the passed function to ${p.toString()}, was not a valid NegFiniteDouble")
      }
    }

    /** Ordering instance for NegFiniteDouble that orders by numeric value. */
    given Ordering[NegFiniteDouble] with {
      def compare(x: NegFiniteDouble, y: NegFiniteDouble): Int = x.compareTo(y)
    }
  }
}
