package org.vennes.yahtzee

import cats.effect.{IO, IOApp}
import org.vennes.yahtzee.animation.instances.given
import cats.Show
import cats.syntax.all.*
import scala.concurrent.duration.*

import org.vennes.yahtzee.types.*

object TestApp extends IOApp.Simple:

  def run: IO[Unit] =
    DiceSet(
      Dice.Side.One,
      Dice.Side.Two,
      Dice.Side.Three,
      Dice.Side.Four,
      Dice.Side.Five
    )
      .animateIO(1.seconds)
