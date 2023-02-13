package org.vennes.yahtzee.types

import cats.syntax.all.*
import cats.Monad
import cats.effect.std.Random

import org.vennes.yahtzee.types.*

case class DiceSet(
    a: Dice.Side,
    b: Dice.Side,
    c: Dice.Side,
    d: Dice.Side,
    e: Dice.Side
):
  def replace(index: DiceSet.Index, value: Dice.Side): DiceSet =
    index match
      case DiceSet.Index.A => this.copy(a = value)
      case DiceSet.Index.B => this.copy(b = value)
      case DiceSet.Index.C => this.copy(c = value)
      case DiceSet.Index.D => this.copy(d = value)
      case DiceSet.Index.E => this.copy(e = value)

  def reRoll[F[_]](
      rolling: List[DiceSet.Index]
  )(using F: Monad[F], Random: Random[F]): F[DiceSet] =
    rolling.foldLeft(this.pure[F]) { case (roundF, index) =>
      Dice.roll[F]().flatMap(value => roundF.map(_.replace(index, value)))
    }

  def toList: List[Dice.Side] =
    List(a, b, c, d, e)

  def toIntList: List[Int] = toList.map(_.toInt)

  def sum: Int =
    toList.map(_.toInt).sum

object DiceSet:

  def create[F[_]]()(using F: Monad[F], Random: Random[F]): F[DiceSet] =
    (
      Dice.roll(),
      Dice.roll(),
      Dice.roll(),
      Dice.roll(),
      Dice.roll()
    ).mapN((a, b, c, d, e) => DiceSet(a, b, c, d, e))

  enum Index:
    case A
    case B
    case C
    case D
    case E

  object Index:
    def from(c: Char): Option[Index] =
      c match
        case 'a' => DiceSet.Index.A.some
        case 'A' => DiceSet.Index.A.some
        case 'b' => DiceSet.Index.B.some
        case 'B' => DiceSet.Index.B.some
        case 'c' => DiceSet.Index.C.some
        case 'C' => DiceSet.Index.C.some
        case 'd' => DiceSet.Index.D.some
        case 'D' => DiceSet.Index.D.some
        case 'e' => DiceSet.Index.E.some
        case 'E' => DiceSet.Index.E.some
        case _   => None
