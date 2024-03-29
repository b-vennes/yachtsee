package org.vennes.yahtzee.animation.instances

import cats.syntax.all.*
import org.vennes.yahtzee.types.*
import cats.Show
import org.vennes.yahtzee.animation.Animation

def drawCard(card: Card): String =
  val topScore = card.topScore
  s"""-------------------------------
    |ones             | ${card.ones.fold("_")(_.show)}
    |twos             | ${card.twos.fold("_")(_.show)}
    |threes           | ${card.threes.fold("_")(_.show)}
    |fours            | ${card.fours.fold("_")(_.show)}
    |fives            | ${card.fives.fold("_")(_.show)}
    |sixes            | ${card.sixes.fold("_")(_.show)}
    |------------------------------
    |top              | ${card.topBonus.fold(card.topScore)(b => s"${card.topScore} + $b")}
    |------------------------------
    |three of a kind  | ${card.threeOak.fold("_")(_.show)}
    |four of a kind   | ${card.fourOak.fold("_")(_.show)}
    |full house:      | ${card.fullHouse.fold("_")(_.show)}
    |small straight   | ${card.smallStraight.fold("_")(_.show)}
    |large straight   | ${card.largeStraight.fold("_")(_.show)}
    |chance           | ${card.chance.fold("_")(_.show)}
    |yahtzee          | ${card.yahtzee.fold("_")(_.show)}
    |bonuses          | ${card.bonuses * 50}
    |-------------------------------
    |bottom           | ${card.bottomScore}
    |-------------------------------
    |total            | ${card.score}
    |-------------------------------""".stripMargin

def animateCard(card: Card): Animation =
  Animation.frame(drawCard(card))

def drawCardWithOptions(card: Card, dice: DiceSet): String =
  def futureScore(value: Int): String = "_ -> " + value

  def optValue(
      value: Card => Option[Int],
      get: DiceSet => Int,
      card: Card,
      dice: DiceSet
  ): String =
    value(card).fold(futureScore(get(dice)))(_.show)

  s"""----------------------------------------
    |ones             | ${optValue(_.ones, Card.scoreOnes, card, dice)}
    |twos             | ${optValue(_.twos, Card.scoreTwos, card, dice)}
    |threes           | ${optValue(_.threes, Card.scoreThrees, card, dice)}
    |fours            | ${optValue(_.fours, Card.scoreFours, card, dice)}
    |fives            | ${optValue(_.fives, Card.scoreFives, card, dice)}
    |sixes            | ${optValue(_.sixes, Card.scoreSixes, card, dice)}
    |-----------------------------------------
    |top              | ${card.topBonus.fold(card.topScore)(b => s"${card.topScore} + $b")}
    |-----------------------------------------
    |three of a kind  | ${optValue(_.threeOak, Card.scoreThreeOak, card, dice)}
    |four of a kind   | ${optValue(_.fourOak, Card.scoreFourOak, card, dice)}
    |full house:      | ${optValue(
      _.fullHouse,
      Card.scoreFullHouse,
      card,
      dice
    )}
    |small straight   | ${optValue(
      _.smallStraight,
      Card.scoreSmallStraight,
      card,
      dice
    )}
    |large straight   | ${optValue(
      _.largeStraight,
      Card.scoreLargeStraight,
      card,
      dice
    )}
    |yahtzee          | ${optValue(_.yahtzee, Card.scoreYahtzee, card, dice)}
    |chance           | ${optValue(_.chance, Card.scoreChance, card, dice)}
    |bonuses          | ${card.bonuses * 50}
    |------------------------------------------
    |bottom           | ${card.bottomScore}
    |------------------------------------------
    |total            | ${card.score}
    |------------------------------------------""".stripMargin

def animateCardWithOptions(card: Card, options: DiceSet): Animation =
  Animation.frame(drawCardWithOptions(card, options))
