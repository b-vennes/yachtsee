package org.vennes.yahtzee.animation.instances

import cats.Show
import org.vennes.yahtzee.types.*
import org.vennes.yahtzee.animation.Animation

given animateDiceSet: Animation[DiceSet] =
  def diceInDiceSet(letter: String): Animation[Dice.Side] =
    Animation
      .concat(
        animateDiceSide,
        Animation.unit(s"     $letter"),
        System.lineSeparator()
      )
      .imap[Dice.Side](_ -> (), _._1)

  Animation
    .interleave(
      diceInDiceSet("A"),
      Animation.interleave(
        diceInDiceSet("B"),
        Animation.interleave(
          diceInDiceSet("C"),
          Animation.interleave(
            diceInDiceSet("D"),
            diceInDiceSet("E"),
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
