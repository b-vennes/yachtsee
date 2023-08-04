package org.vennes.yahtzee.console

import cats.syntax.all.*
import cats.effect.{IO, IOApp}
import cats.effect.std.Random
import org.vennes.yahtzee.types.*
import org.vennes.yahtzee.console.*
import org.vennes.yahtzee.animation.instances.*
import org.vennes.yahtzee.animation.Animation
import scala.concurrent.duration.*

object ConsoleApp extends IOApp.Simple {

  val random: IO[Random[IO]] = Random.scalaUtilRandom[IO]

  object Stages {
    def parseRoll(command: String): List[DiceSet.Index] =
      command
        .replace("roll", "")
        .replace(" ", "")
        .toList
        .flatMap(c => DiceSet.Index.from(c).toList)

    def turnStart(
        turnStart: GameState.TurnStart
    )(using Random[IO]): IO[GameState] =
      if turnStart.card.complete then IO(GameState.GameEnd(turnStart.card))
      else
        for {
          _ <- IO.readLine
          roll <- DiceSet.create[IO]()
        } yield GameState.FirstRoll(turnStart.card, roll)

    def roundOne(
        roundOne: GameState.RoundOne
    )(using Random[IO]): IO[Option[GameState]] = for {
      command <- IO.readLine
      nextState <-
        command.toLowerCase() match {
          case command if command.startsWith("roll") =>
            val reRolling = parseRoll(command)
            roundOne.roll
              .reRoll[IO](reRolling)
              .map(r => GameState.SecondRoll(roundOne.card, r, reRolling).some)
          case command if command.startsWith("card") =>
            GameState
              .Selection(
                roundOne.card,
                roundOne.roll,
                roundOne.some
              )
              .some
              .pure[IO]
          case command if command.contains("q") => None.pure[IO]
          case _                                => roundOne.some.pure[IO]
        }
    } yield nextState

    def roundTwo(
        roundTwo: GameState.RoundTwo
    )(using Random[IO]): IO[Option[GameState]] =
      for {
        command <- IO.readLine
        nextState <-
          command.toLowerCase() match
            case command if command.startsWith("roll") =>
              val reRolling = parseRoll(command)
              roundTwo.roll
                .reRoll[IO](reRolling)
                .map(r => GameState.ThirdRoll(roundTwo.card, r, reRolling).some)
            case command if command.startsWith("card") =>
              GameState
                .Selection(
                  roundTwo.card,
                  roundTwo.roll,
                  GameState.RoundTwo(roundTwo.card, roundTwo.roll).some
                )
                .some
                .pure[IO]
            case command if command.contains("q") => None.pure[IO]
            case _ =>
              GameState.RoundTwo(roundTwo.card, roundTwo.roll).some.pure[IO]
      } yield nextState

    def selection(selection: GameState.Selection): IO[Option[GameState]] =
      for {
        command <- IO.readLine
        nextState <-
          command.toLowerCase() match
            case command if command.startsWith("ch") =>
              Card.Opt
                .from(command.replace("ch", ""))
                .flatMap(selected =>
                  selection.card.withOpt(selected, selection.dice)
                )
                .fold(
                  GameState.Selection(
                    selection.card,
                    selection.dice,
                    selection.previous
                  )
                )(
                  GameState.TurnStart.apply
                )
                .some
                .pure[IO]
            case command if command.startsWith("back") =>
              selection.previous
                .getOrElse(
                  GameState.Selection(
                    selection.card,
                    selection.dice,
                    selection.previous
                  )
                )
                .some
                .pure[IO]
            case command if command.contains("q") => None.pure[IO]
            case _ =>
              GameState
                .Selection(selection.card, selection.dice, selection.previous)
                .some
                .pure[IO]
      } yield nextState

  }

  def handleState(state: GameState)(using
      Random: Random[IO]
  ): IO[Option[GameState]] =
    state match {
      case x: GameState.TurnStart =>
        Stages.turnStart(x).map(_.some)
      case GameState.FirstRoll(card, roll) =>
        IO(GameState.RoundOne(card, roll).some)
      case x: GameState.RoundOne =>
        Stages.roundOne(x)
      case GameState.SecondRoll(card, roll, _) =>
        IO(GameState.RoundTwo(card, roll).some)
      case x: GameState.RoundTwo =>
        Stages.roundTwo(x)
      case GameState.ThirdRoll(card, roll, _) =>
        IO(GameState.Selection(card, roll, None).some)
      case x: GameState.Selection =>
        Stages.selection(x)
      case _ =>
        None.pure[IO]
    }

  def game(state: GameState)(using Random: Random[IO]): IO[Unit] =
    state.tailRecM[IO, Unit](s =>
      for
        _ <- animateGameState(s).animate(0.2.seconds)
        _ <- IO.println("")
        stateOpt <- handleState(s)
        result = stateOpt.fold(().asRight[GameState])(_.asLeft[Unit])
      yield result
    )

  def run: IO[Unit] =
    random.flatMap[Unit](implicit random =>
      game(GameState.TurnStart(Card.initial()))
    )
}
