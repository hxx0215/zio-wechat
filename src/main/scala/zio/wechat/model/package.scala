package zio.wechat


import cats.Show
import cats.implicits._
import io.circe.{Decoder, Encoder, HCursor, Json}
import sttp.tapir.Codec.XmlCodec
import sttp.tapir.{Codec, DecodeResult}
import io.circe.generic.auto._
import io.circe.syntax._



package object model {

  case class ErrorResponse(code: Int, message: String)

  implicit val encodeErrorResponse: Encoder[ErrorResponse] = (e: ErrorResponse) => Json.obj(
    ("errcode", Json.fromInt(e.code)),
    ("errmsg", Json.fromString(e.message))
  )

  implicit val decodeErrorResponse: Decoder[ErrorResponse] = (c: HCursor) => for {
    code <- c.downField("errcode").as[Int]
    message <- c.downField("errmsg").as[String]
  } yield ErrorResponse(code, message)

  case class AccessTokenResponse(accessToken: String, expiresIn: Int)

  implicit val encodeAccessTokenResponse: Encoder[AccessTokenResponse] = (a: AccessTokenResponse) => Json.obj(
    ("access_token", Json.fromString(a.accessToken)),
    ("expires_in", Json.fromInt(a.expiresIn))
  )

  implicit val decodeAccessTokenResponse: Decoder[AccessTokenResponse] = (c: HCursor) => for {
    accessToken <- c.downField("access_token").as[String]
    expiresIn <- c.downField("expires_in").as[Int]
  } yield {
    AccessTokenResponse(accessToken, expiresIn)
  }

  case class WechatAppConfig(appId: String, appSecret: String, token: String, aesKey: String)

  implicit val showWechatMessage: Show[WechatResponseMessage] = Show.show {
    case EmptyMessage => "success"
    case message =>
      val base =
        <xml>
          <ToUserName>
            {scala.xml.PCData(message.toUsername)}
          </ToUserName>
          <FromUserName>
            {scala.xml.PCData(message.fromUsername)}
          </FromUserName>
          <CreateTime>
            {message.createTime}
          </CreateTime>
          <MsgType>
            {scala.xml.PCData(message.msgType)}
          </MsgType>
          <MsgId>
            {message.msgId}
          </MsgId>
        </xml>

      base.copy(child = base.child ++ message.extraFieldToXMLs).toString()
  }

  implicit val wechatRequestMessage: XmlCodec[WechatRequestMessage] = Codec.xml(str => WechatMessage.fromString(str))(_ => "")
  implicit val wechatResponseMessage: XmlCodec[WechatResponseMessage] = Codec.xml(_ => DecodeResult.Missing)(_.show)



  implicit val temporaryQRCodeRequestEncoder: Encoder[TemporaryQRCodeRequest] = (r: TemporaryQRCodeRequest) => Json.obj(
    "expire_seconds" -> Json.fromInt(r.expireSeconds),
    "action_name" -> Json.fromString(r.actionName.entryName),
    "action_info" -> r.actionInfo.asJson
  )
  implicit val temporaryQRCodeRequestDecoder: Decoder[TemporaryQRCodeRequest] = (c: HCursor) => for {
    expireSeconds <- c.downField("expire_seconds").as[Int]
    actionName <- c.downField("action_name").as[String]
    actionInfo <- c.downField("action_info").as[QRCodeActionInfo]
  } yield TemporaryQRCodeRequest(expireSeconds, QRCodeActionName.withName(actionName), actionInfo)

  implicit val permanentQRCodeRequestEncoder: Encoder[PermanentQRCodeRequest] = (r: PermanentQRCodeRequest) => Json.obj(
    "action_name" -> Json.fromString(r.actionName.entryName),
    "action_info" -> r.actionInfo.asJson
  )

  implicit val permanentQRCodeRequestDecoder: Decoder[PermanentQRCodeRequest] = (c: HCursor) => for {
    actionName <- c.downField("action_name").as[String]
    actionInfo <- c.downField("action_info").as[QRCodeActionInfo]
  } yield PermanentQRCodeRequest(QRCodeLimitActionName.withName(actionName), actionInfo)

  implicit val qrCodeIdSceneEncoder: Encoder[QRCodeIdScene] = (idScene: QRCodeIdScene) => Json.obj(
    "scene_id" -> Json.fromInt(idScene.scene)
  )
  implicit val qrCodeStringSceneEncoder: Encoder[QRCodeStringScene] = (stringScene: QRCodeStringScene) => Json.obj(
    "scene_str" -> Json.fromString(stringScene.scene)
  )
  implicit val qrcodeResponseDecoder: Decoder[QRCodeResponse] = (c: HCursor) => for {
    ticket <- c.downField("ticket").as[String]
    expireSeconds <- c.downField("expire_seconds").as[Int]
    url <- c.downField("url").as[String]
  } yield QRCodeResponse(ticket, expireSeconds, url)

  implicit val qrcodeResponseEncoder: Encoder[QRCodeResponse] = (r: QRCodeResponse) => Json.obj(
    "ticket" -> Json.fromString(r.ticket),
    "expire_seconds" -> Json.fromInt(r.expireSeconds),
    "url" -> Json.fromString(r.url)
  )
}
