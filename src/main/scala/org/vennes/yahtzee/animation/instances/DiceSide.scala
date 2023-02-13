package org.vennes.yahtzee.animation.instances

import cats.Show
import org.vennes.yahtzee.types.*
import org.vennes.yahtzee.animation.Animation

given Show[Dice.Side] with
  def show(value: Dice.Side): String =
    value match
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

given animateDiceSide: Animation[Dice.Side] =
  Animation.fromShow[Dice.Side]()
