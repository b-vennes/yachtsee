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

  def parseRoll(command: String): List[DiceSet.Index] =
    command
      .replace("roll", "")
      .replace(" ", "")
      .toList
      .flatMap(c => DiceSet.Index.from(c).toList)

  def handleState(state: GameState)(using
      Random: Random[IO]
  ): IO[Option[GameState]] =
    state match
      case GameState.TurnStart(card) if card.complete =>
        IO.pure(GameState.GameEnd(card).some)
      case GameState.TurnStart(card) =>
        for
          _ <- readLineIO
          roll <- DiceSet.create[IO]()
        yield GameState.RoundOne(card, roll).some
      case GameState.RoundOne(card, roll) =>
        for
          command <- readLineIO
          nextState <-
            command.toLowerCase() match
              case command if command.startsWith("roll") =>
                roll
                  .reRoll[IO](parseRoll(command))
                  .map(r => GameState.RoundTwo(card, r).some)
              case command if command.startsWith("card") =>
                GameState
                  .Selection(
                    card,
                    roll,
                    GameState.RoundOne(card, roll).some
                  )
                  .some
                  .pure[IO]
              case command if command.contains("q") => None.pure[IO]
              case _ => GameState.RoundOne(card, roll).some.pure[IO]
        yield nextState
      case GameState.RoundTwo(card, roll) =>
        for
          command <- readLineIO
          nextState <-
            command.toLowerCase() match
              case command if command.startsWith("roll") =>
                roll
                  .reRoll[IO](parseRoll(command))
                  .map(r => GameState.Selection(card, r, None).some)
              case command if command.startsWith("card") =>
                GameState
                  .Selection(
                    card,
                    roll,
                    GameState.RoundTwo(card, roll).some
                  )
                  .some
                  .pure[IO]
              case command if command.contains("q") => None.pure[IO]
              case _ => GameState.RoundTwo(card, roll).some.pure[IO]
        yield nextState
      case GameState.Selection(card, dice, previous) =>
        for
          command <- readLineIO
          nextState <-
            command.toLowerCase() match
              case command if command.startsWith("ch") =>
                Card.Opt
                  .from(command.replace("ch", ""))
                  .flatMap(selected => card.withOpt(selected, dice))
                  .fold(GameState.Selection(card, dice, previous))(
                    GameState.TurnStart.apply
                  )
                  .some
                  .pure[IO]
              case command if command.startsWith("back") =>
                previous
                  .getOrElse(
                    GameState.Selection(card, dice, previous)
                  )
                  .some
                  .pure[IO]
              case command if command.contains("q") => None.pure[IO]
              case _ =>
                GameState
                  .Selection(card, dice, previous)
                  .some
                  .pure[IO]
        yield nextState
      case _ => None.pure[IO]

  def gameIO(state: GameState)(using Random: Random[IO]): IO[Unit] =
    state.tailRecM[IO, Unit](s =>
      for
        _ <- clearScreen
        _ <- s.animateIO(0.5.seconds)
        _ <- IO.println("")
        stateOpt <- handleState(s)
        result = stateOpt.fold(().asRight[GameState])(_.asLeft[Unit])
      yield result
    )

  def run: IO[Unit] =
    random.flatMap[Unit](implicit random =>
      gameIO(GameState.TurnStart(Card.initial()))
    )
