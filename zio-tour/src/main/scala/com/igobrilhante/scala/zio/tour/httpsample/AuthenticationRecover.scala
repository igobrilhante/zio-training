package com.igobrilhante.scala.zio.tour.httpsample
import zio.{ExitCode, URIO, ZIO}

object AuthenticationRecover extends zio.App {

  def simulateResult(n: Int): ZIO[Any, Int, Int] = {
    n match {
      case 200 =>
        println("Simulation with success")
        ZIO.succeed(200)
      case 401 =>
        println("Simulation with auth error")
        ZIO.fail(401)
      case x =>
        println("Simulation with error")
        ZIO.fail(x)
    }
  }

  def executeWithFallback(
      task: ZIO[Any, Int, Int]
  ): ZIO[Any, Int, Int] = {

    val reAuthenticateTask = ZIO.succeed(200).map { res =>
      println("ReAuthenticationTask")
      res
    }

    def recoverFromFailure(n: Int) =
      n match {
        case 401 => reAuthenticateTask *> task.as(200)
        case x   => ZIO.succeed(x)
      }

    task.flatMap(simulateResult).catchAll(recoverFromFailure)

  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {

    val mainTask = executeWithFallback(ZIO.succeed(200)).map { res =>
      println(s"Result with $res")
      res
    }

    val taskWithReauth = executeWithFallback(ZIO.succeed(401)).map { res =>
      println(s"Result with $res")
      res
    }

    val taskWithException = executeWithFallback(ZIO.succeed(500)).map { res =>
      println(s"Result with $res")
      res
    }

    (taskWithReauth *> taskWithException *> mainTask).exitCode
  }
}
