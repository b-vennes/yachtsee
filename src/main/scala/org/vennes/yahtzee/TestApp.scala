package org.vennes.yahtzee

import cats.effect.{IO, IOApp}
import org.vennes.yahtzee.animation.instances.{*, given}
import org.vennes.yahtzee.animation.Animation
import cats.Show
import cats.syntax.all.*
import scala.concurrent.duration.*
import cats.Id

import org.vennes.yahtzee.types.*
import cats.effect.std.Random
import cats.Applicative

object TestApp extends IOApp.Simple:

  given animate: Animation[DiceSet] = animateDiceSet

  case class DiceState(result: Dice.Side, frames: List[Dice.Side]):
    val animate: (String, Option[DiceState]) =
      frames match
        case next :: tail => next.show -> DiceState(result, tail).some
        case _            => result.show -> None

  val randomIO: IO[Random[IO]] = Random.scalaUtilRandom[IO]

  given diceRollingAnimation: Animation[DiceState] =
    Animation.instance(_.animate)

  def generateFrames[F[_]: Random: Applicative](): F[List[Dice.Side]] =
    (1 to 15).toList.traverse(_ => Dice.roll[F]())

  val clearScreen: IO[Unit] =
    IO.delay(print("\u001b[2J\u001b[1000A\u001b[1000D"))

  def run: IO[Unit] =
    randomIO.flatMap(implicit random =>
      for
        value <- Dice.roll[IO]()
        frames <- generateFrames[IO]()
        _ <- DiceState(value, frames).animateIO(0.1.seconds)
      yield ()
    )
