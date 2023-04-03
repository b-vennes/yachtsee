package org.vennes.yahtzee

import cats.effect.{IO, IOApp}
import org.vennes.yahtzee.animation.instances.{*, given}
import org.vennes.yahtzee.animation.Animation
import cats.Show
import cats.syntax.all.*
import scala.concurrent.duration.*
import cats.Id

import org.vennes.yahtzee.types.*
import cats.effect.std.Random
import cats.Applicative
import org.vennes.yahtzee.types.DiceRoll

object TestApp extends IOApp.Simple:

  val randomIO: IO[Random[IO]] = Random.scalaUtilRandom[IO]

  def run: IO[Unit] =
    randomIO.flatMap { random =>
      given Random[IO] = random

      DiceRoll.make[IO](???, ???)
    }
