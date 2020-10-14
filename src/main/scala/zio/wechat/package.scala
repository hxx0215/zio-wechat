package zio

import zio.wechat.model.WechatAppConfig

package object wechat {
  type WechatAppConfiguration = Has[WechatAppConfig]

  def configLayer(): ULayer[WechatAppConfiguration] = IO.succeed(
    WechatAppConfig("", "", "", "")
  ).toLayer

}
