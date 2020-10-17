package zio.wechat.endpoints

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import sttp.tapir.server.akkahttp._
import zio.wechat.{configLayer, model}
import zio.wechat.server.wechatRequestValidate
import zio.Runtime
import zio.internal.Executor
import zio.wechat.model.{EmptyResponseMessage, ImageRequestMessage, LinkRequestMessage, LocationRequestMessage, ShortVideoRequestMessage, SubscribeEvent, TextRequestMessage, TextResponseMessage, UnsubscribeEvent, VideoRequestMessage, VoiceRequestMessage, WechatRequestMessage, WechatResponseMessage, wechatRequestMessage}

import scala.concurrent.{ExecutionContextExecutor, Future}

object EndpointServerTest extends App {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val dispatcher: ExecutionContextExecutor = actorSystem.getDispatcher
  val runtime = Runtime.default.withExecutor(Executor.fromExecutionContext(1000)(dispatcher))

  import sttp.tapir._

  val hook = new HookEndpoint("web" / "hook")
  val validation: Route = hook.validationEndpoint.toRoute {
    case (_, signature, timestamp, nonce, echostr) =>
      println(s"signature: $signature,timestamp: $timestamp, nonce: $nonce, echostr: $echostr")
      runtime.unsafeRunToFuture(wechatRequestValidate(signature, timestamp, nonce, echostr).provideLayer(configLayer())).map(Right(_))
  }
  val textRoute: Route = hook.wechatMessageEndpoint[WechatRequestMessage, WechatResponseMessage]().toRoute {
    case (_, signature, timestamp, nonce, body) =>
      println(s"signature: $signature,timestamp: $timestamp, nonce: $nonce, body: $body")
      val response: WechatResponseMessage =
        body match {
          case TextRequestMessage(toUsername, fromUsername, createTime, msgId, content) =>
            TextResponseMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "收到文字")
          case ImageRequestMessage(toUsername, fromUsername, createTime, msgId, picUrl, mediaId) =>
            TextResponseMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "图片")
          case VoiceRequestMessage(toUsername, fromUsername, createTime, msgId, format, mediaId, recognition) =>
            TextResponseMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "声音")
          case VideoRequestMessage(toUsername, fromUsername, createTime, msgId, mediaId, thumbMediaId) =>
            TextResponseMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "视频")
          case ShortVideoRequestMessage(toUsername, fromUsername, createTime, msgId, mediaId, thumbMediaId) =>
            TextResponseMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "小视频")
          case LocationRequestMessage(toUsername, fromUsername, createTime, msgId, locationX, locationY, scale, label) =>
            TextResponseMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "位置")
          case LinkRequestMessage(toUsername, fromUsername, createTime, msgId, title, description, url) =>
            TextResponseMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "链接")
          case SubscribeEvent(toUsername, fromUsername, createTime, msgId) =>
            TextResponseMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "欢迎订阅")
          case UnsubscribeEvent(toUsername, fromUsername, createTime, msgId) =>
            EmptyResponseMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId)
        }
      Future.successful(Right(response))
  }

  import akka.http.scaladsl.server.Directives._

  val bindAndCheck = Http().newServerAt("localhost", 8088).bindFlow(validation ~ textRoute)

}
