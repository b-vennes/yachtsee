package org.vennes.yahtzee.console

import cats.syntax.all.*
import cats.effect.{IO, IOApp}
import cats.effect.std.Random
import org.vennes.yahtzee.types.*
import org.vennes.yahtzee.console.*
import org.vennes.yahtzee.animation.instances.given
import org.vennes.yahtzee.animation.Animation
import scala.concurrent.duration.*

object ConsoleApp extends IOApp.Simple:

  val readLineIO: IO[String] =
    IO.interruptible(
      scala.io.StdIn.readLine()
    )

  val random: IO[Random[IO]] = Random.scalaUtilRandom[IO]

  val clearScreen: IO[Unit] =
    IO.delay(print("\u001b[2J\u001b[1000A\u001b[1000D"))

  def handleState(state: GameState)(using
      Random: Random[IO]
  ): IO[Option[GameState]] =
    state match
      case GameState.TurnStart(card) if card.complete =>
        IO.pure(GameState.GameEnd(card).some)
      case GameState.TurnStart(card) =>
        for
          _ <- IO.println(card)
          _ <- readLineIO
          roll <- DiceSet.create[IO]()
        yield GameState.RoundOne(card, roll, List.empty).some
      case GameState.RoundOne(card, roll, keep) =>
        for
          command <- readLineIO
          nextState <-
            command.toLowerCase() match
              case command if command.startsWith("drop") =>
                val keeping =
                  command
                    .replace("drop", "")
                    .replace(" ", "")
                    .toList
                    .flatMap(c => DiceSet.Index.from(c).toList)
                GameState.RoundOne(card, roll, keeping).some.pure[IO]
              case command if command.startsWith("roll") =>
                roll
                  .reRoll[IO](keep)
                  .map(next => GameState.RoundTwo(card, next, List.empty).some)
              case command if command.startsWith("select") =>
                GameState
                  .Selection(
                    card,
                    roll,
                    None,
                    GameState.RoundOne(card, roll, List.empty).some
                  )
                  .some
                  .pure[IO]
              case command if command.contains("q") => None.pure[IO]
              case _ => GameState.RoundOne(card, roll, keep).some.pure[IO]
        yield nextState
      case GameState.RoundTwo(card, roll, keep) =>
        for
          command <- readLineIO
          nextState <-
            command.toLowerCase() match
              case command if command.startsWith("drop") =>
                val keeping =
                  command
                    .replace("drop", "")
                    .replace(" ", "")
                    .toList
                    .flatMap(c => DiceSet.Index.from(c).toList)
                GameState.RoundTwo(card, roll, keeping).some.pure[IO]
              case command if command.startsWith("roll") =>
                roll
                  .reRoll[IO](keep)
                  .map(next => GameState.Selection(card, next, None, None).some)
              case command if command.startsWith("select") =>
                GameState
                  .Selection(
                    card,
                    roll,
                    None,
                    GameState.RoundTwo(card, roll, List.empty).some
                  )
                  .some
                  .pure[IO]
              case command if command.contains("q") => None.pure[IO]
              case _ => GameState.RoundTwo(card, roll, keep).some.pure[IO]
        yield nextState
      case GameState.Selection(card, dice, choosing, previous) =>
        for
          command <- readLineIO
          nextState <-
            command.toLowerCase() match
              case command if command.startsWith("choose") =>
                GameState
                  .Selection(
                    card,
                    dice,
                    Card.Opt.from(command.replace("choose", "")),
                    previous
                  )
                  .some
                  .pure[IO]
              case command if command.startsWith("previous") =>
                previous
                  .getOrElse(
                    GameState.Selection(card, dice, choosing, previous)
                  )
                  .some
                  .pure[IO]
              case command if command.startsWith("confirm") =>
                choosing
                  .flatMap(ch => card.withOpt(ch, dice))
                  .fold(GameState.Selection(card, dice, choosing, previous))(
                    next => GameState.TurnStart(next)
                  )
                  .some
                  .pure[IO]
              case command if command.contains("q") => None.pure[IO]
              case _ =>
                GameState
                  .Selection(card, dice, choosing, previous)
                  .some
                  .pure[IO]
        yield nextState
      case _ => None.pure[IO]

  def gameIO(state: GameState)(using Random: Random[IO]): IO[Unit] =
    state.tailRecM[IO, Unit](s =>
      for
        _ <- clearScreen
        _ <- s.animateIO(0.5.seconds)
        stateOpt <- handleState(s)
        result = stateOpt.fold(().asRight[GameState])(_.asLeft[Unit])
      yield result
    )

  def run: IO[Unit] =
    random.flatMap[Unit](implicit random =>
      gameIO(GameState.TurnStart(Card.initial()))
    )
