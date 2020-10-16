package zio

import zio.wechat.model.WechatAppConfig

package object wechat {
  type WechatAppConfiguration = Has[WechatAppConfig]

  def configLayer(): ULayer[WechatAppConfiguration] = IO.succeed(
    WechatAppConfig("wx3595fa5d8cafd522", "a53837bd6f161226111efe79c4f0445a", "aibasis", "MhY1mquKE9cQMDO3tcBBLycCyAv8PrmhgS7irgJaSYg")
  ).toLayer

}
