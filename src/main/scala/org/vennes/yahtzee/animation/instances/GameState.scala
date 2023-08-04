package org.vennes.yahtzee.animation.instances

import org.vennes.yahtzee.animation.Animation
import org.vennes.yahtzee.animation.instances.{*, given}
import org.vennes.yahtzee.types.*
import cats.syntax.all.*
import cats.Show
import org.vennes.yahtzee.types.GameState.TurnStart
import cats.effect.std.Random
import cats.effect.IO

object Hints {
  val viewCard: String =
    "Say 'card' to choose a spot on the card for this roll."

  val roll: String = "Say 'roll <a> <b> <c> <d> <e>' to re-roll specific dice."
}

def animateTurnStart(start: GameState.TurnStart): Animation =
  Animation.stack(
    animateCard(start.card),
    Animation.break,
    Animation.frame("Press <enter> to roll")
  )

def animateFirstRoll(roll: GameState.FirstRoll)(using Random[IO]): Animation =
  Animation.stack(
    Animation.frame("First Roll:"),
    animateDiceRoll(
      roll.roll,
      List(
        DiceSet.Index.A,
        DiceSet.Index.B,
        DiceSet.Index.C,
        DiceSet.Index.D,
        DiceSet.Index.E
      )
    )
  )

def animateRoundOne(roundOne: GameState.RoundOne)(using Random[IO]): Animation =
  Animation.stack(
    Animation.frame("First Roll:"),
    animateDiceSet(roundOne.roll),
    animateCard(roundOne.card),
    Animation.frame(Hints.roll),
    Animation.frame(Hints.viewCard)
  )

def animateSecondRoll(roll: GameState.SecondRoll)(using Random[IO]): Animation =
  Animation.stack(
    Animation.frame("Second Roll:"),
    animateDiceRoll(roll.roll, roll.rolling)
  )

def animateRoundTwo(roundTwo: GameState.RoundTwo)(using Random[IO]): Animation =
  Animation.stack(
    Animation.frame("Second Roll:"),
    animateDiceSet(roundTwo.roll),
    animateCard(roundTwo.card),
    Animation.frame(Hints.roll),
    Animation.frame(Hints.viewCard)
  )

def animateThirdRoll(roll: GameState.ThirdRoll)(using Random[IO]): Animation =
  Animation.stack(
    Animation.frame("Final Roll:"),
    animateDiceRoll(roll.roll, roll.rolling)
  )

def animateSelection(selection: GameState.Selection): Animation =
  Animation.stack(
    Animation.frame("Final Roll:"),
    animateDiceSetNoLetters(selection.dice),
    Animation.frame("Select spot:"),
    animateCardWithOptions(selection.card, selection.dice),
    Animation.frame("Say 'ch <spot>' to select a spot to play your roll."),
    Animation.frame(
      selection.previous.fold("")(_ => "Say 'back' to go back to rolling.")
    )
  )

def animateGameEnd(gameEnd: GameState.GameEnd): Animation =
  Animation.stack(
    Animation.frame("Game over!"),
    animateCard(gameEnd.card)
  )

def animateGameState(gameState: GameState)(using Random[IO]): Animation =
  gameState match
    case x: TurnStart            => animateTurnStart(x)
    case x: GameState.FirstRoll  => animateFirstRoll(x)
    case x: GameState.RoundOne   => animateRoundOne(x)
    case x: GameState.SecondRoll => animateSecondRoll(x)
    case x: GameState.RoundTwo   => animateRoundTwo(x)
    case x: GameState.ThirdRoll  => animateThirdRoll(x)
    case x: GameState.Selection  => animateSelection(x)
    case x: GameState.GameEnd    => animateGameEnd(x)
