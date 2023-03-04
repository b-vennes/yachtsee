package org.vennes.yahtzee.animation.types

import cats.syntax.all.{*, given}
import org.vennes.yahtzee.types.Dice
import cats.effect.std.Random
import cats.Applicative

case class DiceRoll(result: Dice.Side, frames: List[Dice.Side])

object DiceRoll:

  def make[F[_]: Applicative](result: Dice.Side, frameCount: Int)(using Random: Random[F]): F[DiceRoll] =
    (1 to frameCount).toList
      .traverse(_ => Dice.roll[F]())
      .map(frames =>
        DiceRoll(
          result,
          frames
        )
      )
