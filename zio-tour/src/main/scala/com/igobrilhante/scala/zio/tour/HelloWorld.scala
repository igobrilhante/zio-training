package com.igobrilhante.scala.zio.tour
import com.igobrilhante.scala.zio.tour.ErrorRecovery.failed
import zio.console._
import zio.{App, ExitCode, URIO, ZEnv, ZIO}

object HelloWorld extends App {
  def run(args: List[String]) = putStrLn("Hello World").exitCode
}

object PrintSequence extends App {
  def run(args: List[String]) = {
    putStrLn("Hello World 1") *>
      putStrLn("Hello World 2") *>
      ZIO.succeed(0)
  }.exitCode
}

object ErrorRecovery extends App {

  val failed = putStrLn("About to fail ...") *>
    ZIO.fail("Failed!") *>
    putStrLn("Nothing happens here ...")

  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    (failed.as(0) orElse ZIO.succeed(0)).exitCode

}

object Logging extends App {

  def repeat[R, E, A](int: Int)(effect: ZIO[R, E, A]): ZIO[R, E, A] = {
    int match {
      case x if x <= 1 => effect
      case x           => effect *> repeat(x - 1)(effect)
    }
  }

  def run(args: List[String]) = {
    repeat(100)(putStrLn("Effect")).exitCode
  }

}
