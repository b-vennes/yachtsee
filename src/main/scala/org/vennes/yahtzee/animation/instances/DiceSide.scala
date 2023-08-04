package org.vennes.yahtzee.animation.instances

import cats.Show
import org.vennes.yahtzee.types.*
import org.vennes.yahtzee.animation.Animation
import cats.effect.std.Random
import cats.effect.IO
import cats.syntax.all.*
import cats.data.NonEmptyList

def drawDiceSide(side: Dice.Side): String =
  side match
    case Dice.Side.One =>
      s"""  -------
         | |       |
         | |   o   |
         | |       |
         |  ------- """.stripMargin
    case Dice.Side.Two =>
      s"""  -------
         | | o     |
         | |       |
         | |     o |
         |  ------- """.stripMargin
    case Dice.Side.Three =>
      s"""  -------
         | | o     |
         | |   o   |
         | |     o |
         |  ------- """.stripMargin
    case Dice.Side.Four =>
      s"""  -------
         | | o   o |
         | |       |
         | | o   o |
         |  ------- """.stripMargin
    case Dice.Side.Five =>
      s"""  -------
         | | o   o |
         | |   o   |
         | | o   o |
         |  ------- """.stripMargin
    case Dice.Side.Six =>
      s"""  -------
         | | o o o |
         | |       |
         | | o o o |
         |  ------- """.stripMargin

def animateDiceSide(side: Dice.Side): Animation =
  Animation.frame(drawDiceSide(side))

def animateRoll(side: Dice.Side, spins: Int)(using Random[IO]): Animation =
  Animation.framesIO(
    (1 to spins).toList
      .traverse(_ => Dice.roll[IO]())
      .map(sides =>
        NonEmptyList.fromListUnsafe(
          (sides ++ List.fill(3)(side)).map(drawDiceSide)
        )
      )
  )
