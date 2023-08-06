package org.vennes.yahtzee.animation

import cats.*
import cats.syntax.all.*
import cats.effect.*
import scala.concurrent.duration.FiniteDuration
import cats.data.NonEmptyList

case class Animation(get: IO[(String, Option[Animation])]) {

  def frame: IO[String] = get.map(_._1)
  def next: IO[Option[Animation]] = get.map(_._2)

  val clearScreenCode = "\u001b[2J\u001b[1000A\u001b[1000D"

  def animate(interval: FiniteDuration): IO[Unit] =
    for {
      _ <- IO.print(clearScreenCode)
      frameValue <- frame
      _ <- IO.println(frameValue)
      nextValue <- next
      _ <- nextValue.fold(IO.unit)(a =>
          IO.sleep(interval).flatMap(_ => a.animate(interval))
      )
    } yield ()
}

object Animation {
  def frame(frame: String): Animation =
    Animation(IO.delay(frame -> None))

  def unfold[A](
      initial: A,
      frame: A => IO[String],
      next: A => Option[A]
  ): Animation =
    Animation(
      frame(initial).map(_ -> next(initial).map(unfold(_, frame, next)))
    )

  def pure(frame: String, next: Option[Animation]): Animation =
    Animation(IO.delay(frame -> next))

  def row(
      joinWith: String,
      first: Animation,
      second: Animation,
      others: Animation*
  ): Animation =
    val combined =
      Animation(
        (first.get, second.get).mapN((a, b) =>
          val aSplit = a._1.split(System.lineSeparator()).toList
          val bSplit = b._1.split(System.lineSeparator()).toList

          val frame = interleaveLists(
            aSplit,
            bSplit,
            aSplit.maxBy(_.length).length,
            joinWith,
            System.lineSeparator()
          )

          val nextAnimation = (a._2, b._2) match {
            case (Some(aAnim), Some(bAnim)) => row(joinWith, aAnim, bAnim).some
            case (Some(aAnim), None) =>
              row(joinWith, aAnim, Animation.frame(b._1)).some
            case (None, Some(bAnim)) =>
              row(joinWith, Animation.frame(a._1), bAnim).some
            case (None, None) => None
          }

          frame -> nextAnimation
        )
      )

    NonEmptyList
      .fromList(others.toList)
      .fold(combined)(x => row(joinWith, combined, x.head, x.tail: _*))

  def stack(
      first: Animation,
      second: Animation,
      others: Animation*
  ): Animation =
    val combined =
      Animation(
        (first.get, second.get).mapN((a, b) =>
          val frame = (a._1 + System.lineSeparator() + b._1)
          val nextAnimation = (a._2, b._2) match {
            case (Some(aAnim), Some(bAnim)) => stack(aAnim, bAnim).some
            case (Some(aAnim), None) => stack(aAnim, Animation.frame(b._1)).some
            case (None, Some(bAnim)) => stack(Animation.frame(a._1), bAnim).some
            case (None, None)        => None
          }
          frame -> nextAnimation
        )
      )
    NonEmptyList
      .fromList(others.toList)
      .fold(combined)(x => stack(combined, x.head, x.tail: _*))

  def break: Animation =
    Animation.frame(System.lineSeparator())

  def frames(values: NonEmptyList[String]): Animation =
    NonEmptyList
      .fromList(values.tail)
      .fold(Animation.pure(values.head, None))(t =>
        Animation.pure(values.head, frames(t).some)
      )

  def framesIO(valuesIO: IO[NonEmptyList[String]]): Animation =
    Animation(
      for {
        values <- valuesIO
        current = values.head
        nonEmptyTailOpt = NonEmptyList.fromList(values.tail)
        next = nonEmptyTailOpt.fold(None)(t => frames(t).some)
      } yield current -> next
    )

  /**
      * A helper function to interleave two lists of strings with a given string between them.
      * Useful for the [[row]] function.
      *
      * @param a the first list of strings
      * @param b the second list of strings
      * @param aMax the length of the longest string in the original first list
      * @param between the string to put between the two lists
      * @param after the string to put after each combination
      * @return the interleaved string
      */
  private def interleaveLists(
      a: List[String],
      b: List[String],
      aMax: Int,
      between: String,
      after: String
  ): String =
    (a, b) match {
      case (aHead :: aTail, bHead :: bTail) =>
        aHead.padTo(aMax, ' ') + between + bHead + after + interleaveLists(
          aTail,
          bTail,
          aMax,
          between,
          after
        )
      case (Nil, bHead :: bTail) =>
        "".padTo(aMax, ' ') + between + bHead + after + interleaveLists(
          Nil,
          bTail,
          aMax,
          between,
          after
        )
      case (aHead :: aTail, Nil) =>
        aHead.padTo(aMax, ' ') + between + after + interleaveLists(
          aTail,
          Nil,
          aMax,
          between,
          after
        )
      case (Nil, Nil) => ""
    }
}
