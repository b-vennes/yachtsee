package org.vennes.yahtzee.types

import cats.syntax.all.{*, given}
import org.vennes.yahtzee.types.Dice
import cats.effect.std.Random
import cats.Applicative
import org.vennes.yahtzee.animation.Animation

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

  given animation: Animation[DiceRoll] =
    Animation.instance(state => 
      state.frames match
        case next :: remaining => next.show -> Some(DiceRoll(state.result, state.frames))
        case _ => state.result.show -> None
    )
