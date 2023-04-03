package org.vennes.yahtzee.animation.instances

import cats.syntax.all.*
import cats.{Show, Applicative}
import org.vennes.yahtzee.types.*
import org.vennes.yahtzee.animation.Animation
import cats.effect.std.Random

def diceInDiceSet(letterOpt: Option[String])(using DiceSideAnimation: Animation[Dice.Side]): Animation[Dice.Side] =
  letterOpt.fold(DiceSideAnimation)((letter: String) =>
    Animation
      .concat(
        DiceSideAnimation,
        Animation.unit(s"     $letter"),
        System.lineSeparator()
      )
      .imap[Dice.Side](_ -> (), _._1)
  )

val animateDiceSet: Animation[DiceSet] =
  Animation
    .interleave(
      diceInDiceSet("A".some),
      Animation.interleave(
        diceInDiceSet("B".some),
        Animation.interleave(
          diceInDiceSet("C".some),
          Animation.interleave(
            diceInDiceSet("D".some),
            diceInDiceSet("E".some),
            System.lineSeparator(),
            " "
          ),
          System.lineSeparator(),
          " "
        ),
        System.lineSeparator(),
        " "
      ),
      System.lineSeparator(),
      " "
    )
    .imap(
      diceSet =>
        diceSet.a -> (diceSet.b -> (diceSet.c -> (diceSet.d -> diceSet.e))),
      values =>
        DiceSet(
          values._1,
          values._2._1,
          values._2._2._1,
          values._2._2._2._1,
          values._2._2._2._2
        )
    )

val animateDiceSetNoLetters: Animation[DiceSet] =
  Animation
    .interleave(
      diceInDiceSet(None),
      Animation.interleave(
        diceInDiceSet(None),
        Animation.interleave(
          diceInDiceSet(None),
          Animation.interleave(
            diceInDiceSet(None),
            diceInDiceSet(None),
            System.lineSeparator(),
            " "
          ),
          System.lineSeparator(),
          " "
        ),
        System.lineSeparator(),
        " "
      ),
      System.lineSeparator(),
      " "
    )
    .imap(
      diceSet =>
        diceSet.a -> (diceSet.b -> (diceSet.c -> (diceSet.d -> diceSet.e))),
      values =>
        DiceSet(
          values._1,
          values._2._1,
          values._2._2._1,
          values._2._2._2._1,
          values._2._2._2._2
        )
    )

def generateRollingFrames[F[_]: Random: Applicative](): F[List[Dice.Side]] =
  (1 to 10).toList.traverse(_ => Dice.roll[F]())
