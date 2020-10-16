package zio.wechat.endpoints

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import sttp.tapir.server.akkahttp._
import zio.wechat.{configLayer, model}
import zio.wechat.server.wechatRequestValidate
import zio.Runtime
import zio.internal.Executor
import zio.wechat.model.{ImageRequestMessage, TextRequestMessage, VoiceRequestMessage, WechatRequestMessage, WechatResponseMessage, wechatRequestMessage}

import scala.concurrent.{ExecutionContextExecutor, Future}

object EndpointServerTest extends App {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val dispatcher: ExecutionContextExecutor = actorSystem.getDispatcher
  val runtime = Runtime.default.withExecutor(Executor.fromExecutionContext(1000)(dispatcher))
  //  val hookRoute: Route = webHook("web" / "hook").toRoute {
  //    case (_, signature, timestamp, nonce, echostr,body) =>
  //      println(s"signature: $signature,timestamp: $timestamp, nonce: $nonce, echostr: $echostr")
  //      println(s"body:$body")
  //      runtime.unsafeRunToFuture(wechatRequestValidate(signature, timestamp, nonce, echostr).provideLayer(configLayer())).map(Right(_))
  //  }

  import sttp.tapir._

  val hook = new HookEndpoint("web" / "hook")
  val validation: Route = hook.validationEndpoint.toRoute {
    case (_, signature, timestamp, nonce, echostr) =>
      println(s"signature: $signature,timestamp: $timestamp, nonce: $nonce, echostr: $echostr")
      runtime.unsafeRunToFuture(wechatRequestValidate(signature, timestamp, nonce, echostr).provideLayer(configLayer())).map(Right(_))
  }
  val textRoute: Route = hook.wechatMessageEndpoint().toRoute {
    case (_, signature, timestamp, nonce, body) =>
      println(s"signature: $signature,timestamp: $timestamp, nonce: $nonce, body: $body")
      val response =
        body match {
          case TextRequestMessage(toUsername, fromUsername, createTime, msgId, content) =>
            TextRequestMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "收到文字")
          case ImageRequestMessage(toUsername, fromUsername, createTime, msgId, picUrl, mediaId) =>
            TextRequestMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "图片")
          case VoiceRequestMessage(toUsername, fromUsername, createTime, msgId, format, mediaId, recognition) =>
            TextRequestMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "声音")
          case _ =>
            WechatRequestMessage
        }
      Future.successful(Right(response))

  import akka.http.scaladsl.server.Directives._

  val bindAndCheck = Http().newServerAt("localhost", 8088).bindFlow(validation ~ textRoute)

}
