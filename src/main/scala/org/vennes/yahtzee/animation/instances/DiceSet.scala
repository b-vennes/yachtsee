package org.vennes.yahtzee.animation.instances

import cats.syntax.all.*
import cats.{Show, Applicative}
import org.vennes.yahtzee.types.*
import org.vennes.yahtzee.animation.Animation
import cats.effect.std.Random
import cats.effect.IO

def animateDiceInDiceSet(
    diceSide: Dice.Side,
    letterOpt: Option[String]
): Animation =
  letterOpt.fold(animateDiceSide(diceSide))((letter: String) =>
    Animation
      .stack(
        animateDiceSide(diceSide),
        Animation.frame(s"     $letter")
      )
  )

def animateDiceSet(diceSet: DiceSet): Animation =
  Animation
    .row(
      " ",
      animateDiceInDiceSet(diceSet._1, "A".some),
      animateDiceInDiceSet(diceSet._2, "B".some),
      animateDiceInDiceSet(diceSet._3, "C".some),
      animateDiceInDiceSet(diceSet._4, "D".some),
      animateDiceInDiceSet(diceSet._5, "E".some)
    )

def animateDiceSetNoLetters(diceSet: DiceSet): Animation =
  Animation
    .row(
      " ",
      animateDiceInDiceSet(diceSet._1, None),
      animateDiceInDiceSet(diceSet._2, None),
      animateDiceInDiceSet(diceSet._3, None),
      animateDiceInDiceSet(diceSet._4, None),
      animateDiceInDiceSet(diceSet._5, None)
    )

def rollingFrames(using Random[IO]): IO[List[Dice.Side]] =
  (1 to 10).toList.traverse(_ => Dice.roll[IO]())

def animateDiceRoll(diceSet: DiceSet, rolling: List[DiceSet.Index])(using
    Random[IO]
): Animation =
  Animation.row(
    " ",
    if rolling.contains(DiceSet.Index.A) then animateRoll(diceSet._1, 6)
    else animateDiceSide(diceSet._1),
    if rolling.contains(DiceSet.Index.B) then animateRoll(diceSet._2, 10)
    else animateDiceSide(diceSet._2),
    if rolling.contains(DiceSet.Index.C) then animateRoll(diceSet._3, 14)
    else animateDiceSide(diceSet._3),
    if rolling.contains(DiceSet.Index.D) then animateRoll(diceSet._4, 16)
    else animateDiceSide(diceSet._4),
    if rolling.contains(DiceSet.Index.E) then animateRoll(diceSet._5, 20)
    else animateDiceSide(diceSet._5)
  )
