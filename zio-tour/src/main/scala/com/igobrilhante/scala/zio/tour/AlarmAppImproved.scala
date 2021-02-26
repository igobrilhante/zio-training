package com.igobrilhante.scala.zio.tour
import java.io.IOException
import java.util.concurrent.TimeUnit

import zio.{ExitCode, RIO, URIO, ZIO}
import zio.blocking.Blocking
import zio.console.{getStrLn, putStr, putStrLn, Console}
import zio.duration.{durationInt, Duration}
import zio.random.{nextIntBounded, Random}

object AlarmAppImproved extends zio.App {

  def toDouble(s: String): Either[NumberFormatException, Double] =
    try Right(s.toDouble)
    catch { case e: NumberFormatException => Left(e) }

  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String) =
      toDouble(input).map(double =>
        Duration((double * 1000.0).toLong, TimeUnit.MILLISECONDS)
      )

    val fallback =
      putStrLn("You didn't enter the number of seconds!") *> getAlarmDuration

    for {
      _ <- putStrLn("Please enter the number of seconds to sleep: ")
      input <- getStrLn
      duration <- ZIO.fromEither(parseDuration(input)).orElse(fallback)
    } yield duration
  }

  def getRandom: ZIO[Random, Nothing, String] =
    for {
      random <- nextIntBounded(11)
      string = if (random < 3) "Z" else "z"
    } yield (string)

  def printRandom: ZIO[Console with Random, Throwable, Unit] =
    for {
      random <- getRandom
      _ <- putStr(random)
    } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    (for {
      duration <- getAlarmDuration
      fiber <- (printRandom *> ZIO.sleep(1.second)).forever.fork
      _ <- ZIO.sleep(duration)
      _ <- putStrLn("")
      _ <- putStrLn("Time to wake up!!!")
      _ <- fiber.interrupt
    } yield 0).exitCode
  }
}
