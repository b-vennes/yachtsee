import scala.scalanative.build._

ThisBuild / organization := "org.vennes"
ThisBuild / scalaVersion := "3.3.0"

lazy val catsEffectVersion = "3.5.1"

lazy val yahtzee = crossProject(JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("."))
    .settings(
      libraryDependencies ++= Seq(
        // "core" module - IO, IOApp, schedulers
        // This pulls in the kernel and std modules automatically.
        "org.typelevel" %%% "cats-effect" % catsEffectVersion,
        // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
        "org.typelevel" %%% "cats-effect-kernel" % catsEffectVersion,
        // standard "effect" library (Queues, Console, Random etc.)
        "org.typelevel" %%% "cats-effect-std" % catsEffectVersion,
      )
    )
    .nativeSettings(
      nativeConfig ~= { config =>
        config
          .withMode(Mode.releaseSize)
      }
    )

lazy val root = project
  .in(file("."))
  .aggregate(yahtzee.jvm, yahtzee.native)
  .settings(
    publishLocal := {},
    publish := {},
  )
