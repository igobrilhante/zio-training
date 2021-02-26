package com.igobrilhante.scala.zio.tour
import zio.{ExitCode, Ref, URIO, ZIO}
import zio.console.{getStrLn, putStr, putStrLn, Console}
import zio.random.{nextDouble, Random}
import zio.duration._
object ComputePi extends zio.App {

  final case class PiState(inside: Long, total: Long)

  def estimatePi(inside: Long, total: Long): Double =
    (inside.toDouble / total.toDouble) * 4.0

  def insideCircle(x: Double, y: Double): Boolean =
    Math.sqrt(x * x + y * y) <= 1.0

  def randomPoint: ZIO[Random, Nothing, (Double, Double)] =
    nextDouble.zip(nextDouble)

  def updateOnce(ref: Ref[PiState]): ZIO[Random, Nothing, Unit] =
    for {
      tuple <- randomPoint
      (x, y) = tuple
      inside = if (insideCircle(x, y)) 1 else 0
      _ <- ref.update(state => PiState(state.inside + inside, state.total + 1))
    } yield ()

  def printEstimate(ref: Ref[PiState]): ZIO[Console, Nothing, Unit] = {
    for {
      state <- ref.get
      _ <- putStrLn(s"Current state: $state")
      _ <- putStrLn(s"Pi ~ ${estimatePi(state.inside, state.total)}")
    } yield ()
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    (for {
      ref <- Ref.make(PiState(0L, 0L))
      worker = updateOnce(ref).forever
      workers = List.fill(4)(worker)
      fiber1 <- ZIO.forkAll(workers)
      fiber2 <- (printEstimate(ref) *> ZIO.sleep(10.seconds)).forever.fork
      _ <- putStrLn("Enter any key to terminate...")
      _ <- getStrLn *> putStrLn("Terminating...") *>
          (fiber1 zip fiber2).interrupt
    } yield 0).exitCode

}
