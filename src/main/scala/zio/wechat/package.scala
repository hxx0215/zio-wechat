package zio

import sttp.client.asynchttpclient.zio.{AsyncHttpClientZioBackend, SttpClient}
import zio.wechat.model.WechatAppConfig
import sttp.client._
import sttp.tapir.client.sttp._

package object wechat {
  type WechatAppConfiguration = Has[WechatAppConfig]

  def configLayer(): ULayer[WechatAppConfiguration] = IO.succeed(
    //    WechatAppConfig("wx3595fa5d8cafd522", "a53837bd6f161226111efe79c4f0445a", "aibasis", "MhY1mquKE9cQMDO3tcBBLycCyAv8PrmhgS7irgJaSYg")
    WechatAppConfig("wxaf99fd18d929117e", "bd46a71de4f309bc20fc98fb144b7e30", "aibasis", "MhY1mquKE9cQMDO3tcBBLycCyAv8PrmhgS7irgJaSYg")
  ).toLayer

  def clientLayer(): TaskLayer[SttpClient] = AsyncHttpClientZioBackend.layer()

  def accessTokenLayer =
    ZIO.accessM[SttpClient with WechatAppConfiguration](ctx => {
      val client = ctx.get[SttpClient.Service]
      val config = ctx.get[WechatAppConfig]
      import zio.wechat.endpoints._
      client.send(accessToken.toSttpRequestUnsafe(uri"https://api.weixin.qq.com").apply("client_credential", config.appId, config.appSecret))
    }).toLayer

}
