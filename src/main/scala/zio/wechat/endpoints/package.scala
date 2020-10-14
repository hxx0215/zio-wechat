package zio.wechat

import io.circe.parser.decode
import io.circe.syntax._
import sttp.tapir._
import zio.wechat.model.{AccessTokenResponse, ErrorResponse}

package object endpoints {
  val mapOut: String => Either[ErrorResponse, AccessTokenResponse] = (out: String) => {
    decode[AccessTokenResponse](out) match {
      case Right(value) => Right[ErrorResponse, AccessTokenResponse](value)
      case Left(_) =>
        val Right(e) = decode[ErrorResponse](out)
        Left[ErrorResponse, AccessTokenResponse](e)
    }
  }
  val accessToken = endpoint.get.in("cgi-bin" / "token")
    .in(query[String]("grant_type")).description("获取access_token填写client_credential")
    .in(query[String]("appid")).description("第三方用户唯一凭证")
    .in(query[String]("secret")).description("第三方用户唯一凭证密钥，即appsecret")
    .out(stringBody).errorOut(stringBody)
    .mapOut(mapOut)({
      case Right(v) => v.asJson.toString()
      case Left(v) => v.asJson.toString()
    })

  val webHook = endpoint.get.in("web"/"hook")
    .in(query[String]("signature")).description("微信加密签名，signature结合了开发者填写的token参数和请求中的timestamp参数、nonce参数。")
    .in(query[String]("timestamp")).description("时间戳")
    .in(query[String]("nonce")).description("随机数")
    .in(query[String]("echostr")).description("随机字符串")
    .out(stringBody).errorOut(stringBody)
}
