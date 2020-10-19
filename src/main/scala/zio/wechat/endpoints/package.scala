package zio.wechat

import io.circe.{Decoder, Encoder}
import io.circe.parser.decode
import io.circe.syntax._
import sttp.tapir._
import zio.wechat.model.{AccessTokenResponse, ErrorResponse, QRCodeResponse, TemporaryQRCodeRequest}
import sttp.tapir.json.circe.jsonBody

package object endpoints {
  def mapOut[T: Decoder]() = (out: String) =>{
    decode[T](out) match {
      case Right(value) => Right[ErrorResponse,T](value)
      case Left(_) =>
        val Right(e) = decode[ErrorResponse](out)
        Left[ErrorResponse, T](e)
    }
  }
  def mapping[T: Decoder: Encoder]: Mapping[String, Either[ErrorResponse, T]] = Mapping.from(mapOut[T]())({
    case Right(v) => v.asJson.toString()
    case Left(v) => v.asJson.toString()
  })


  val accessToken = endpoint.get.in("cgi-bin" / "token")
    .in(query[String]("grant_type")).description("获取access_token填写client_credential")
    .in(query[String]("appid")).description("第三方用户唯一凭证")
    .in(query[String]("secret")).description("第三方用户唯一凭证密钥，即appsecret")
    .out(stringBody).errorOut(stringBody)
    .mapOut(mapping[AccessTokenResponse])

  val temporaryQRCode = endpoint.post.in("cgi-bin" / "qrcode" / "create")
    .in(query[String]("access_token"))
    .in(jsonBody[TemporaryQRCodeRequest])
    .out(stringBody).errorOut(stringBody)
    .mapOut(mapping[QRCodeResponse])

  class HookEndpoint[T](i: EndpointInput[T]) {
    private val baseEndpoint = endpoint.in(i)
      .in(query[String]("signature")).description("微信加密签名，signature结合了开发者填写的token参数和请求中的timestamp参数、nonce参数。")
      .in(query[String]("timestamp")).description("时间戳")
      .in(query[String]("nonce")).description("随机数")
    val validationEndpoint = baseEndpoint.get.in(query[String]("echostr")).description("随机字符串")
      .out(stringBody).errorOut(stringBody)

    def wechatMessageEndpoint[WechatRequestMessage: Codec.XmlCodec, WechatResponseMessage: Codec.XmlCodec]() =
      baseEndpoint.post.in(xmlBody[WechatRequestMessage]).out(xmlBody[WechatResponseMessage]).errorOut(stringBody)

  }


}
