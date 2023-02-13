package org.vennes.yahtzee.animation.instances

import org.vennes.yahtzee.animation.Animation
import org.vennes.yahtzee.animation.instances.{*, given}
import org.vennes.yahtzee.types.*
import cats.syntax.all.*
import cats.Show
import org.vennes.yahtzee.types.GameState.TurnStart

val animateTurnStart: Animation[GameState.TurnStart] =
  animateCard
    .between(
      """New Game:
        |""".stripMargin,
      """<enter> to roll
        |""".stripMargin
    )
    .imap[GameState.TurnStart](
      turnStart => turnStart.card,
      card => GameState.TurnStart(card)
    )

val animateRoundOne: Animation[GameState.RoundOne] =
  animateDiceSet
    .between(
      """First Roll:
        |""".stripMargin,
      """
        |""".stripMargin
    )
    .concatNewline(
      Animation
        .static[List[DiceSet.Index]](selected =>
          if selected.isEmpty then "say 'drop <a><b><c><d><e>' to re-roll dice"
          else s"Dropping -- ${selected.mkString(" ")}"
        )
        .between(
          "",
          """say 'select' to choose a spot for this roll
            |say 'roll' to roll the dice you aren't keeping""".stripMargin
        )
    )
    .concatEmpty[Card]()
    .imap[GameState.RoundOne](
      roundOne => (roundOne.roll -> roundOne.keep) -> roundOne.card,
      state => GameState.RoundOne(state._2, state._1._1, state._1._2)
    )

val animateRoundTwo: Animation[GameState.RoundTwo] =
  animateDiceSet
    .between(
      """Second Roll:
        |""".stripMargin,
      """
        |""".stripMargin
    )
    .concatNewline(
      Animation
        .static[List[DiceSet.Index]](selected =>
          if selected.isEmpty then "say 'drop <a><b><c><d><e>' to re-roll dice"
          else s"Dropping -- ${selected.mkString(" ")}"
        )
        .between(
          "",
          """say 'select' to choose a spot for this roll
            |say 'roll' to roll the dice you aren't keeping""".stripMargin
        )
    )
    .concatEmpty[Card]()
    .imap[GameState.RoundTwo](
      roundTwo => (roundTwo.roll -> roundTwo.keep) -> roundTwo.card,
      state => GameState.RoundTwo(state._2, state._1._1, state._1._2)
    )

val animateSelection: Animation[GameState.Selection] =
  animateCardWithOptions
    .between(
      s"""Select Spot:
        |""".stripMargin,
      ""
    )
    .concatNewline(
      Animation.static[Option[Card.Opt]](cardOpt =>
        cardOpt.fold("")(c => s"selecting $c")
      )
    )
    .concatNewline(
      Animation.static[Option[GameState]](previous =>
        previous.fold("")(_ => "say 'previous' to go back to rolling")
      )
    )
    .imap[GameState.Selection](
      selection => ((selection.card, selection.dice, selection.choose) -> selection.choose) -> selection.previous,
      values => GameState.Selection(values._1._1._1, values._1._1._2, values._1._1._3, values._2)
    )

val animateGameEnd: Animation[GameState.GameEnd] =
  animateCard
    .between(
      s"""Game Over!
        |""".stripMargin,
      ""
    )
    .imap(
      gameEnd => gameEnd.card,
      card => GameState.GameEnd(card)
    )

given animateGameState: Animation[GameState] =
  Animation
    .either(
      animateTurnStart,
      Animation.either(
        animateRoundOne,
        Animation.either(
          animateRoundTwo,
          Animation.either(
            animateSelection,
            animateGameEnd
          )
        )
      )
    )
    .imap[GameState](
      gameState => gameState match
        case turnStart: GameState.TurnStart =>
          Left(turnStart)
        case roundOne: GameState.RoundOne =>
          Right(Left(roundOne))
        case roundTwo: GameState.RoundTwo =>
          Right(Right(Left(roundTwo)))
        case selection: GameState.Selection =>
          Right(Right(Right(Left(selection))))
        case gameEnd: GameState.GameEnd =>
          Right(Right(Right(Right(gameEnd)))),
      eithers => eithers match
        case Left(turnStart) => turnStart
        case Right(Left(roundOne)) => roundOne
        case Right(Right(Left(roundTwo))) => roundTwo
        case Right(Right(Right(Left(selection)))) => selection
        case Right(Right(Right(Right(gameEnd)))) => gameEnd
    )
