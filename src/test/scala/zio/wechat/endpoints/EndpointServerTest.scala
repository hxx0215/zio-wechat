package zio.wechat.endpoints

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import sttp.tapir.server.akkahttp._
import zio.wechat.configLayer
import zio.wechat.server.wechatRequestValidate
import zio.Runtime

import scala.concurrent.ExecutionContextExecutor

object EndpointServerTest extends App{

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val dispatcher: ExecutionContextExecutor = actorSystem.getDispatcher
  val runtime = Runtime.default
  val hookRoute: Route = webHook.toRoute{
    case (signature, timestamp , nonce, echostr) =>
      println(s"signature: $signature,timestamp: $timestamp, nonce: $nonce, echostr: $echostr")
      runtime.unsafeRun(wechatRequestValidate(signature,timestamp, nonce, echostr).provideLayer(configLayer()).toFuture).map(Right(_))
  }
  val bindAndCheck = Http().newServerAt("localhost", 8088).bindFlow(hookRoute)

}
