package zio.wechat.endpoints

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import sttp.tapir.server.akkahttp._
import zio.Runtime
import zio.internal.Executor
import zio.wechat.{configLayer, model}
import zio.wechat.model.{ClickEvent, ImageMessage, LinkMessage, LocationEvent, LocationMessage, MusicResponseMessage, NewsArticle, NewsResponseMessage, ScanEvent, ShortVideoMessage, SubscribeEvent, TextMessage, UnsubscribeEvent, VideoRequestMessage, VideoResponseMessage, ViewEvent, VoiceRequestMessage, VoiceResponseMessage, WechatMessage, WechatRequestMessage, WechatResponseMessage}
import zio.wechat.server.wechatRequestValidate

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
      val response: WechatResponseMessage = {
        val currentTime = (System.currentTimeMillis() / 1000).toInt
        body match {
          case TextMessage(toUsername, fromUsername, createTime, msgId, content) =>
            TextMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "收到文字")
          case ImageMessage(toUsername, fromUsername, createTime, msgId, picUrl, mediaId) =>
            ImageMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, picUrl, mediaId)
          case VoiceRequestMessage(toUsername, fromUsername, createTime, msgId, format, mediaId, recognition) =>
            VoiceResponseMessage(fromUsername, toUsername, currentTime, msgId, mediaId = mediaId)
          case VideoRequestMessage(toUsername, fromUsername, createTime, msgId, mediaId, thumbMediaId) =>
            VideoResponseMessage(fromUsername, toUsername, currentTime, msgId, "6kHJ175k-Iy0-d3aT4tJr0uKp2fcv2aQlg0qRw7-RdvmzgFVx4N0EGpvwkKMZXmz", "hello", "description")
          case ShortVideoMessage(toUsername, fromUsername, createTime, msgId, mediaId, thumbMediaId) =>
            TextMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "小视频")
          case LocationMessage(toUsername, fromUsername, createTime, msgId, locationX, locationY, scale, label) =>
//            TextMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "位置")
            MusicResponseMessage(fromUsername,toUsername,currentTime,msgId,"音乐","描述","https://shadow-iris.oss-us-west-1.aliyuncs.com/ASCA%20-%20RESISTER%20-%20%E8%AF%95%E5%90%AC%E7%89%88.mp3","https://shadow-iris.oss-us-west-1.aliyuncs.com/ASCA%20-%20RESISTER%20-%20%E8%AF%95%E5%90%AC%E7%89%88.mp3","NcuayE8MavWK8vBtpx7OhgSENj17Li8O2o7SHfjqgeVjUu1ZnlLWB5lMj9_ECOWB")
          case LinkMessage(toUsername, fromUsername, createTime, msgId, title, description, url) =>
            NewsResponseMessage(fromUsername,toUsername,currentTime,msgId,1,Seq(NewsArticle("图文","图文描述","https://shadow-iris.oss-us-west-1.aliyuncs.com/akkachanjp_2017-Mar-01.jpg","https://www.baidu.com")))
          case SubscribeEvent(toUsername, fromUsername, createTime, msgId, eventKey, ticket) =>
            TextMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "关注")
          case UnsubscribeEvent(toUsername, fromUsername, createTime, msgId) =>
            TextMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, "取消关注")
          case LocationEvent(toUsername, fromUsername, _, msgId, latitude, longitude, precision) =>
            TextMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, s"位置信息:la:${latitude},lo:${longitude},pre:${precision}")
          case ViewEvent(toUsername, fromUsername, _, msgId, eventKey) =>
            TextMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, s"view event $eventKey")
          case ClickEvent(toUsername, fromUsername, createTime, msgId, eventKey) =>
            TextMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, s"click event:$eventKey")
          case ScanEvent(toUsername, fromUsername, createTime, msgId, eventKey, ticket) =>
            TextMessage(fromUsername, toUsername, (System.currentTimeMillis() / 1000).toInt, msgId, s"scan event:$eventKey ticket:$ticket")
        }
      }
      Future.successful(Right(response))
  }

  import akka.http.scaladsl.server.Directives._

  val bindAndCheck = Http().newServerAt("localhost", 8088).bindFlow(validation ~ textRoute)

}
