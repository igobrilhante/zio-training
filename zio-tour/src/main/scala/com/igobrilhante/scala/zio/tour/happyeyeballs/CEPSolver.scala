package com.igobrilhante.scala.zio.tour.happyeyeballs

import sttp.client3.{basicRequest, Response, UriContext}
import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import zio.{ExitCode, URIO, ZIO}
import zio.console.{getStrLn, putStrLn}
import zio.duration.durationInt

object CEPSolver extends zio.App {

  def brasilAPI(cep: String): String =
    "https://brasilapi.com.br/api/cep/v1/${cep}"
  def viaCEP(cep: String): String = s"https://viacep.com.br/ws/${cep}/json"

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {

    val myAppLogic =
      for {
        _ <- putStrLn("Hello! Type the Brazilian CEP below")
        cep <- getStrLn
        _ <- searchCEP(cep)
      } yield ()

    myAppLogic.exitCode

  }

  private def searchCEP(cep: String) = {
    val tasks = ZIO
      .succeed(List(brasilAPI(cep), viaCEP(cep)))
      .map { list =>
        list.map { baseUri =>
          AsyncHttpClientZioBackend().flatMap { backend =>
            val request = basicRequest.get(uri"${baseUri}")

            ZIO.succeed(
              println(s"Request to ${baseUri} at ${System.currentTimeMillis()}")
            ) *>
              backend.send(request)
          }
        }
      }

    tasks
      .flatMap { tasks =>
        ReleasableHappyEyeballs(tasks, 1000.milliseconds, close)
      }
      .map { response =>
        println("")
        println("RESULT")
        println(s"Uri: ${response.request.uri}")
        println(s"Code: ${response.code}")
        response.body match {
          case Right(value) => println(value)
          case Left(value)  => println(value)
        }
      }
  }

  private def close(result: Response[Either[String, String]]) =
    ZIO.effect(result).ignore
}
