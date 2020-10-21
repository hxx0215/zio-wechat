package zio

import zio.wechat.model.{AccessTokenResponse, WechatAppConfig}

package object wechat {
  type WechatAppConfiguration = Has[WechatAppConfig]
  type WechatAccessToken = Has[AccessTokenResponse]

  def wechatConfig = IO.succeed(
    //    WechatAppConfig("wx3595fa5d8cafd522", "a53837bd6f161226111efe79c4f0445a", "aibasis", "MhY1mquKE9cQMDO3tcBBLycCyAv8PrmhgS7irgJaSYg")
    WechatAppConfig("wxaf99fd18d929117e", "bd46a71de4f309bc20fc98fb144b7e30", "aibasis", "MhY1mquKE9cQMDO3tcBBLycCyAv8PrmhgS7irgJaSYg")
  )



}
