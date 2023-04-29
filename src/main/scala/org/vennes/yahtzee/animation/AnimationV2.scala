package org.vennes.yahtzee.animation

import cats.*
import cats.data.*
import cats.syntax.all.{*, given}
import cats.effect.IO
import scala.concurrent.duration.FiniteDuration

case class AnimationV2[F[_]](get: F[String], next: OptionT[F, AnimationV2[F]]):
  def appendThen(s: String, other: AnimationV2[F])(using F: Monad[F]): AnimationV2[F] =
    (this, AnimationV2.static(s), other).fold

  def append(other: AnimationV2[F])(using F: Monad[F]): AnimationV2[F] =
    (this, other).fold

  def appendLineThen(other: AnimationV2[F])(using F: Monad[F]): AnimationV2[F] =
    appendThen(System.lineSeparator(), other)

  def interleavedLines(other: AnimationV2[F])(using F: Monad[F]): AnimationV2[F] =
    AnimationV2(
      (get, other.get).mapN((a, b) =>
        val separator = System.lineSeparator()
        val aSplit = a.split(separator).toList
        val aLength = aSplit.map(_.length).max

        val bSplit = b.split(separator).toList

        interleaveValues(aSplit, bSplit, aLength, separator)
      ),
      AnimationV2.mergeNexts(this, other)
    )

  def animateIO(rate: FiniteDuration)(using ToIO: ~>[F, IO]): IO[Unit] =
    this
      .tailRecM[IO, Unit](state =>
        for
          display <- ToIO(state.get)
          _ <- IO.delay(print("\u001b[2J\u001b[1000A\u001b[1000D"))
          _ <- IO.println(display)
          next <- ToIO(state.next.value)
          _ <- next.fold(IO.unit)(_ => IO.sleep(rate))
        yield next.toLeft(())
      )

  private def interleaveValues(
      a: List[String],
      b: List[String],
      aMax: Int,
      between: String
  ): String =
    (a, b) match
      case (aHead :: aTail, bHead :: bTail) =>
        aHead.padTo(aMax, ' ') + between + bHead + interleaveValues(
          aTail,
          bTail,
          aMax,
          between
        )
      case (Nil, bHead :: bTail) =>
        "".padTo(aMax, ' ') + between + bHead + interleaveValues(
          Nil,
          bTail,
          aMax,
          between
        )
      case (aHead :: aTail, Nil) =>
        aHead.padTo(aMax, ' ') + between + interleaveValues(
          aTail,
          Nil,
          aMax,
          between
        )
      case (Nil, Nil) => ""

object AnimationV2:
  def empty[F[_]](using F: Applicative[F]): AnimationV2[F] =
    AnimationV2(F.pure(""), OptionT.none[F, AnimationV2[F]])

  def static[F[_]](value: String)(using F: Applicative[F]): AnimationV2[F] =
    AnimationV2(F.pure(value), OptionT.none[F, AnimationV2[F]])

  def show[F[_], A](value: A)(using F: Applicative[F], A: Show[A]): AnimationV2[F] =
    AnimationV2(F.pure(A.show(value)), OptionT.none[F, AnimationV2[F]])

  def mergeNexts[F[_]](first: AnimationV2[F], second: AnimationV2[F])(using F: Monad[F]): OptionT[F, AnimationV2[F]] =
    OptionT(
      first.next.fold(
        second.next.fold(None)(oN =>
          oN.some
        )
      )(n =>
        second.next.fold(
          n.some
        )(oN => n.append(oN).some)
      ).flatten
    )

  def merge[F[_]](first: AnimationV2[F], second: AnimationV2[F])(using F: Monad[F]): AnimationV2[F] =
    AnimationV2(
      first.get.map(_ + second.get),
      mergeNexts(first, second)
    )

  given [F[_]](using F: Monad[F]): Monoid[AnimationV2[F]] with
    def empty: AnimationV2[F] = AnimationV2.empty
    def combine(x: AnimationV2[F], y: AnimationV2[F]): AnimationV2[F] = AnimationV2.merge(x, y)
