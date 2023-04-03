package org.vennes.yahtzee.types

import cats.syntax.all.*
import cats.{Applicative, Show}
import cats.effect.std.Random
import scala.runtime.AbstractFunction4
import org.vennes.yahtzee.animation.Animation

object Dice:

  enum Side:
    case One
    case Two
    case Three
    case Four
    case Five
    case Six

  object Side:
    given Show[Side] with
      def show(value: Side): String =
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

    given Animation[Side] =
      Animation.fromShow[Dice.Side]()

  val sides: List[Side] =
    List(Side.One, Side.Two, Side.Three, Side.Four, Side.Five, Side.Six)

  def roll[F[_]]()(using F: Applicative[F], Random: Random[F]): F[Side] =
    Random
      .shuffleList(sides)
      .map(_.head)

  extension (side: Side)
    def toInt: Int =
      side match
        case Side.One   => 1
        case Side.Two   => 2
        case Side.Three => 3
        case Side.Four  => 4
        case Side.Five  => 5
        case Side.Six   => 6
