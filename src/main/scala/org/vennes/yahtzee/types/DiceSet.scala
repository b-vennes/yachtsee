package org.vennes.yahtzee.types

import cats.syntax.all.*
import cats.{Monad, Applicative}
import cats.effect.std.Random
import org.vennes.yahtzee.animation.Animation

import org.vennes.yahtzee.types.*

case class DiceSet(
    a: Dice.Side,
    b: Dice.Side,
    c: Dice.Side,
    d: Dice.Side,
    e: Dice.Side
)

extension (self: DiceSet)

  def replace(index: DiceSet.Index, value: Dice.Side): DiceSet =
    index match
      case DiceSet.Index.A => self.copy(a = value)
      case DiceSet.Index.B => self.copy(b = value)
      case DiceSet.Index.C => self.copy(c = value)
      case DiceSet.Index.D => self.copy(d = value)
      case DiceSet.Index.E => self.copy(e = value)

  def reRoll[F[_]](
      roll: List[DiceSet.Index]
  )(using F: Monad[F], Random: Random[F]): F[DiceSet] =
    roll.foldLeft(self.pure[F]) { case (roundF, index) =>
      Dice.roll[F]().flatMap(value => roundF.map(_.replace(index, value)))
    }

  def toList: List[Dice.Side] =
    List(self.a, self.b, self.c, self.d, self.e)

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

  val indexList: List[Index] = List(
    Index.A,
    Index.B,
    Index.C,
    Index.D,
    Index.E
  )

  def diceInDiceSet(label: String)(using DiceSideAnimation: Animation[Dice.Side]): Animation[Dice.Side] =
    Animation
      .concat(
        DiceSideAnimation,
        Animation.unit(s"     $label"),
        System.lineSeparator()
      )
      .imap[Dice.Side](_ -> (), _._1)

  given Animation[DiceSet] =
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

  def generateRollingFrames[F[_]: Random: Applicative](): F[List[Dice.Side]] =
    (1 to 10).toList.traverse(_ => Dice.roll[F]())
