package com.igobrilhante.scala.zio.tour.happyeyeballs
import java.net.{InetAddress, Socket}

import zio.{ExitCode, URIO, ZIO}
import zio.blocking.{effectBlocking, Blocking}
import zio.console.putStrLn
import zio.duration.durationInt

object HappyEyeBallsTest extends zio.App {

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {

    val tasks = effectBlocking {
      val inetAddress = InetAddress.getAllByName("debian.org").toList
      println(inetAddress)
      inetAddress
    }

    tasks
      .map { addresses =>
        addresses.map(a => effectBlocking(new Socket(a, 443)))
      }
      .flatMap { tasks =>
        println(s"Number of tasks: ${tasks.length}")
        ReleasableHappyEyeballs(tasks, 250.milliseconds, closeSocket)
      }
      .tapBoth(
        error => putStrLn(s"ERROR: $error"),
        v => putStrLn(s"Connected: ${v.getInetAddress}")
      )
      .fold(_ => ExitCode.success, _ => ExitCode.failure)

  }

  def closeSocket(s: Socket): ZIO[Blocking, Nothing, Unit] =
    effectBlocking(s.close()).ignore
}
