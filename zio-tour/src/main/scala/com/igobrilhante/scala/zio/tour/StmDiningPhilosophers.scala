package com.igobrilhante.scala.zio.tour

import zio._
import zio.console._
import zio.stm._

object StmDiningPhilosophers extends zio.App {

  final case class Fork(number: Int)

  final case class Placement(
      left: TRef[Option[Fork]],
      right: TRef[Option[Fork]]
  )

  final case class Roundtable(seats: Vector[Placement])

  def takeForks(
      left: TRef[Option[Fork]],
      right: TRef[Option[Fork]]
  ): STM[Nothing, (Fork, Fork)] = {

    for {
      leftFork <- left.get.collect {
        case Some(fork) => fork
      }
      _ <- left.set(None)
      rightFork <- right.get.collect {
        case Some(fork) =>
          fork
      }
      _ <- right.set(None)
    } yield (leftFork, rightFork)
  }

  def putForks(left: TRef[Option[Fork]], right: TRef[Option[Fork]])(
      tuple: (Fork, Fork)
  ): STM[Nothing, Unit] = {
    val (leftFork, rightFork) = tuple

    for {
      _ <- left.set(Some(leftFork))
      _ <- right.set(Some(rightFork))
    } yield ()

  }

  def displayRoundtable(roundtable: Roundtable): ZIO[Console, Nothing, Unit] = {
    for {
      _ <- putStrLn(s"Seats: ${roundtable.seats.mkString("\t\n")}")
    } yield ()
  }

  def setupTable(size: Int): ZIO[Any, Nothing, Roundtable] = {
    def makeFork(i: Int) = TRef.make[Option[Fork]](Some(Fork(i)))

    (for {
      allForks0 <- STM.foreach((0 to size).toVector)(i => makeFork(i))
      allForks = allForks0 ++ List(allForks0(0))
      placements = (allForks zip allForks.drop(1)).map {
        case (l, r) => Placement(l, r)
      }

    } yield {
      Roundtable(placements)
    }).commit

  }

  def eat(
      philosopher: Int,
      roundtable: Roundtable
  ): ZIO[Console, Nothing, Unit] = {
    val placement = roundtable.seats(philosopher)

    val left = placement.left
    val right = placement.right

    for {
      forks <- takeForks(left, right).commit
      _ <- putStrLn(s"Philosopher ${philosopher} eating...")
      _ <- putForks(left, right)(forks).commit
      _ <- putStrLn(s"Philosopher ${philosopher} is done eating")
    } yield ()

  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val count = 10

    def eaters(table: Roundtable): Iterable[ZIO[Console, Nothing, Unit]] =
      (0 to count).map { index => eat(index, table) }

    for {
      table <- setupTable(count)
      fiber <- ZIO.forkAll(eaters(table))
      _ <- fiber.join
      _ <- putStrLn("All philosophers have eaten!")
    } yield ExitCode.success
  }

}
