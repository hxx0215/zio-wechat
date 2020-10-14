package zio.wechat

import io.circe.{Decoder, Encoder, HCursor, Json}

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

}
