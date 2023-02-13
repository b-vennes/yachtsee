package org.vennes.yahtzee.animation

import cats.syntax.all.*
import cats.Show
import cats.effect.IO
import scala.concurrent.duration.FiniteDuration

trait Animation[A]:

  extension (value: A)
    def unconsFrame(): (String, Option[A])

    def getFrame(): String = unconsFrame()._1

    def getNext(): Option[A] = unconsFrame()._2

    def animateIO(rate: FiniteDuration): IO[Unit] =
      value
        .tailRecM[IO, Unit](state =>
          IO.println(state.getFrame()) *>
            state
              .getNext()
              .fold(IO(().asRight))(next => IO(next.asLeft))
              .flatTap(_.fold(_ => IO.sleep(rate), _ => IO.unit))
        )

  def imap[B](toA: B => A, toB: A => B): Animation[B] =
    Animation.instance[B] { b =>
      val a = toA(b)
      val (frame, nextA) = a.unconsFrame()
      frame -> nextA.map(toB)
    }

  def concat[B](other: Animation[B], seperator: String): Animation[(A, B)] =
    Animation.concat(this, other, seperator)

  def concatNewline[B](other: Animation[B]): Animation[(A, B)] =
    Animation.concatNewline(this, other)

  def concatEmpty[B](): Animation[(A, B)] =
    Animation.concatEmpty(this)

  def interleave[B](other: Animation[B], seperator: String, splitOn: String): Animation[(A, B)] =
    Animation.interleave(this, other, splitOn, seperator)

  def frameMap(f: String => String): Animation[A] =
    Animation.instance[A](a =>
      val (str, nextA) = a.unconsFrame()
      f(str) -> nextA
    )

  def between(left: String, right: String): Animation[A] =
    Animation.between(this, left, right)

object Animation:

  def instance[A](f: A => (String, Option[A])): Animation[A] =
    new Animation[A]:
      extension (value: A)
        def unconsFrame(): (String, Option[A]) =
          f(value)

  def fromShow[A: Show](): Animation[A] =
    static(_.show)

  def static[A](f: A => String): Animation[A] =
    instance(f(_) -> None)

  def unit(value: String): Animation[Unit] =
    instance(_ => value -> None)

  def const[A](value: String): Animation[A] =
    instance(_ => value -> None)

  def empty[A](): Animation[A] =
    instance(_ => "" -> None)

  def concatEmpty[A, B](value: Animation[A]): Animation[(A, B)] =
    concat(value, Animation.empty[B](), "")

  private def tupledOpts[A, B](
      aOpt: Option[A],
      bOpt: Option[B],
      fillA: A,
      fillB: B
  ): Option[(A, B)] =
    (aOpt, bOpt) match
      case (Some(a), Some(b)) => Some((a, b))
      case (None, Some(b))    => Some((fillA, b))
      case (Some(a), None)    => Some((a, fillB))
      case (None, None)       => None

  def concat[A, B](
      a: Animation[A],
      b: Animation[B],
      between: String
  ): Animation[(A, B)] =
    Animation.instance(value =>
      given Animation[A] = a
      given Animation[B] = b

      val (aStr, nextA) = value._1.unconsFrame()
      val (bStr, nextB) = value._2.unconsFrame()

      aStr + between + bStr -> tupledOpts(nextA, nextB, value._1, value._2)
    )

  def concatNewline[A, B](
    a: Animation[A],
    b: Animation[B]
  ): Animation[(A, B)] =
    concat(a, b, System.lineSeparator())

  private def interleaveValues(
      a: List[String],
      b: List[String],
      aMax: Int,
      between: String,
      after: String
  ): String =
    (a, b) match
      case (aHead :: aTail, bHead :: bTail) =>
        aHead.padTo(aMax, ' ') + between + bHead + after + interleaveValues(
          aTail,
          bTail,
          aMax,
          between,
          after
        )
      case (Nil, bHead :: bTail) =>
        "".padTo(aMax, ' ') + between + bHead + after + interleaveValues(
          Nil,
          bTail,
          aMax,
          between,
          after
        )
      case (aHead :: aTail, Nil) =>
        aHead.padTo(aMax, ' ') + between + after + interleaveValues(
          aTail,
          Nil,
          aMax,
          between,
          after
        )
      case (Nil, Nil) => ""

  def interleave[A, B](
      a: Animation[A],
      b: Animation[B],
      on: String,
      between: String
  ): Animation[(A, B)] =
    Animation.instance(value =>
      given Animation[A] = a
      given Animation[B] = b

      val (aStr, nextA) = value._1.unconsFrame()
      val (bStr, nextB) = value._2.unconsFrame()

      val aSplit = aStr.split(on).toList
      val aLength = aSplit.map(_.length).max

      val bSplit = bStr.split(on).toList

      interleaveValues(aSplit, bSplit, aLength, between, on) -> tupledOpts(
        nextA,
        nextB,
        value._1,
        value._2
      )
    )

  def wrap[A](f: A => Animation[A]): Animation[A] =
    Animation.instance[A](a =>
      given Animation[A] = f(a)
      a.unconsFrame()
    )

  def between[A](animA: Animation[A], left: String, right: String): Animation[A] =
    Animation.instance[A](a =>
      import animA.*

      val (str, nextA) = a.unconsFrame()

      (left + str + right, nextA)
    )

  def either[A, B](left: Animation[A], right: Animation[B]): Animation[Either[A, B]] =
    Animation.instance[Either[A, B]](either =>
      either.fold(
        a =>
          import left.*
          val (strA, nextA) = a.unconsFrame()
          strA -> nextA.map(_.asLeft),
        b =>
          import right.*
          val (strB, nextB) = b.unconsFrame()
          strB -> nextB.map(_.asRight)
      )
    )
