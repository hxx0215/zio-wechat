package zio.wechat

import io.circe.{Decoder, Encoder}
import io.circe.parser.decode
import io.circe.syntax._
import sttp.tapir._
import zio.wechat.model.{AccessTokenResponse, CustomMenuConfig, CustomSupportAvatarForm, ErrorResponse, MainMenu, MenuId, MenuInformation, QRCodeRequest, QRCodeResponse, RemoveCustomSupportMessage, UpdateCustomSupportMessage}
import sttp.tapir.json.circe.jsonBody

package object endpoints {
  def mapOut[T: Decoder]() = (out: String) => {
    val result = decode[T](out)
    result match {
      case Right(value) => Right[ErrorResponse, T](value)
      case Left(_) =>
        val Right(e) = decode[ErrorResponse](out)
        Left[ErrorResponse, T](e)
    }
  }

  def mapping[T: Decoder : Encoder]: Mapping[String, Either[ErrorResponse, T]] = Mapping.from(mapOut[T]())({
    case Right(v) => v.asJson.toString()
    case Left(v) => v.asJson.toString()
  })


  val accessToken = endpoint.get.in("cgi-bin" / "token")
    .in(query[String]("grant_type")).description("获取access_token填写client_credential")
    .in(query[String]("appid")).description("第三方用户唯一凭证")
    .in(query[String]("secret")).description("第三方用户唯一凭证密钥，即appsecret")
    .out(stringBody).errorOut(stringBody)
    .mapOut(mapping[AccessTokenResponse])


  def baseOutEndpoint[T: Decoder : Encoder] = endpoint.out(stringBody).errorOut(stringBody).mapOut(mapping[T])

  val accessTokenQuery = query[String]("access_token")

  def generateQRCodeEndpoint[M <: QRCodeRequest : Encoder : Decoder : Schema : Validator] =
    endpoint.post.in("cgi-bin" / "qrcode" / "create")
      .in(accessTokenQuery)
      .in(jsonBody[M]).out(stringBody).errorOut(stringBody).mapOut(mapping[QRCodeResponse])

  val showQRCodeEndpoint = endpoint.get.in("cgi-bin" / "showqrcode").in(query[String]("ticket"))
    .out(fileBody).errorOut(stringBody)

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

  val createMenuEndpoint =
  //    baseOutEndpoint[ErrorResponse]
    endpoint.out(stringBody)
      .post.in("cgi-bin" / "menu" / "create").in(accessTokenQuery)
      .in(jsonBody[MainMenu])

  val fetchMenuEndpoint = endpoint.get.in("cgi-bin" / "get_current_selfmenu_info").in(accessTokenQuery).out(stringBody).errorOut(stringBody).mapOut(mapping[MenuInformation])

  val deleteMenuEndpoint = baseOutEndpoint[ErrorResponse].get.in("cgi-bin" / "menu" / "delete").in(accessTokenQuery)
    .out(stringBody).errorOut(stringBody)

  val createCustomMenuEndpoint = baseOutEndpoint[MenuId].post.in("cgi-bin" / "menu" / "addconditional").in(accessTokenQuery)
    .in(jsonBody[MainMenu])

  val deleteCustomMenuEndpoint = baseOutEndpoint[ErrorResponse].post.in("cgi-bin" / "menu" / "delconditional").in(accessTokenQuery)
    .in(jsonBody[MenuId])


  val fetchMenuConfigEndpoint = baseOutEndpoint[CustomMenuConfig].get.in("cgi-bin" / "menu" / "get").in(accessTokenQuery)


  val createCustomSupportEndpoint = baseOutEndpoint[ErrorResponse].post.in("customservice" / "kfaccount" / "add").in(accessTokenQuery)
    .in(jsonBody[UpdateCustomSupportMessage])

  val updateCustomSupportEndpoint = baseOutEndpoint[ErrorResponse].post.in("customservice" / "kfaccount" / "update").in(accessTokenQuery)
    .in(jsonBody[UpdateCustomSupportMessage])

  val removeCustomSupportEndpoint = baseOutEndpoint[ErrorResponse].post.in("customservice" / "kfaccount" / "del").in(accessTokenQuery)
    .in(jsonBody[RemoveCustomSupportMessage])

  val uploadCustomSupportAvatarEndpoint = baseOutEndpoint[ErrorResponse].post.in("customservice" / "kfaccount" / "uploadheadimg").in(accessTokenQuery)
    .in(query[String]("kf_account")).in(multipartBody[CustomSupportAvatarForm])



}
