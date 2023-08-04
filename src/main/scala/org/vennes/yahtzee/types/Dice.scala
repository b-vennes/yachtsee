package org.vennes.yahtzee.types

import cats.syntax.all.*
import cats.Applicative
import cats.effect.std.Random
import scala.runtime.AbstractFunction4

object Dice:

  enum Side {
    case One
    case Two
    case Three
    case Four
    case Five
    case Six
  }

  val sides: List[Side] =
    List(Side.One, Side.Two, Side.Three, Side.Four, Side.Five, Side.Six)

  def roll[F[_]]()(using F: Applicative[F], Random: Random[F]): F[Side] =
    Random
      .shuffleList(sides)
      .map(_.head)

  extension (side: Side)
    def toInt: Int =
      side match {
        case Side.One   => 1
        case Side.Two   => 2
        case Side.Three => 3
        case Side.Four  => 4
        case Side.Five  => 5
        case Side.Six   => 6
      }
