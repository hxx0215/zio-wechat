package zio.wechat


import cats.{Monoid, Show}
import cats.implicits._
import io.circe.{Decoder, Encoder, HCursor, Json}
import sttp.tapir.{Codec, DecodeResult}

import scala.xml._
import shapeless.syntax.std.tuple._
import sttp.tapir.Codec.XmlCodec


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

  trait WechatMessage {
    def toUsername: String

    def fromUsername: String

    def createTime: Int

    def msgType: String

    def extraFieldToXMLs: Seq[Elem]

    def msgId: Long
  }

  case object EmptyMessage extends WechatMessage {
    def extraFieldToXMLs: Seq[Elem] = Seq.empty

    override def msgType: String = ""

    override def toUsername: String = ""

    override def fromUsername: String = ""

    override def createTime: Int = 0

    override def msgId: Long = 0L
  }

  object WechatMessage {
    def baseMessage(root: Elem): (String, String, Int, Long) = {
      (
        (root \ "ToUserName").text,
        (root \ "FromUserName").text,
        (root \ "CreateTime").text.toInt,
        Option((root \ "MsgId").text).filter(_.trim.nonEmpty).map(_.toLong).getOrElse(0L)
        // Event do not have msg id
      )
    }

    def fromString(xmlString: String): DecodeResult[WechatMessage] = {
      val root = XML.loadString(xmlString)
      (root \ "MsgType").text match {
        case "text" => DecodeResult.Value {
          TextMessage tupled baseMessage(root) :+ (root \ "Content").text
        }
        case "image" =>
          DecodeResult.Value(
            ImageMessage tupled baseMessage(root) :+ (root \ "PicUrl").text :+ (root \ "MediaId").text
          )
        case "voice" =>
          DecodeResult.Value(
            VoiceMessage tupled baseMessage(root) :+ (root \ "Format").text :+ (root \ "MediaId").text :+ (root \ "Recognition").headOption.map(_.text)
          )
        case "video" =>
          DecodeResult.Value(
            VideoMessage tupled baseMessage(root) :+ (root \ "MediaId").text :+ (root \ "ThumbMediaId").text
          )
        case "shortvideo" =>
          DecodeResult.Value(
            ShortVideoMessage tupled baseMessage(root) :+ (root \ "MediaId").text :+ (root \ "ThumbMediaId").text
          )
        case "location" =>
          DecodeResult.Value(
            LocationMessage tupled baseMessage(root) :+ (root \ "Location_X").text.toDouble :+ (root \ "Location_Y").text.toDouble :+ (root \ "Scale").text.toDouble :+ (root \ "Label").text
          )
        case "link" =>
          DecodeResult.Value(
            LinkMessage tupled baseMessage(root) :+ (root \ "Title").text :+ (root \ "Description").text :+ (root \ "Url").text
          )
        case "event" =>
          (root \ "Event").text match {
            case "subscribe" =>
              DecodeResult.Value(
                SubscribeEvent tupled baseMessage(root)
              )
            case "unsubscribe" =>
              DecodeResult.Value(
                UnsubscribeEvent tupled baseMessage(root)
              )
          }
        case _ => DecodeResult.Missing
      }
    }
  }

  implicit val showWechatMessage: Show[WechatMessage] = Show.show {
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

  implicit val wechatMessage: XmlCodec[WechatMessage] = Codec.xml(WechatMessage.fromString)(_.show)

  case class TextMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, content: String) extends WechatMessage {
    override val msgType: String = "text"

    def extraFieldToXMLs = Seq(
      <Content>
        {scala.xml.PCData(content)}
      </Content>
    )
  }

  case class ImageMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, picUrl: String, mediaId: String) extends WechatMessage {
    override val msgType: String = "image"

    def extraFieldToXMLs = Seq(
      <PicUrl>
        {scala.xml.PCData(picUrl)}
      </PicUrl>,
      <MediaId>
        {scala.xml.PCData(mediaId)}
      </MediaId>
    )
  }

  case class VoiceMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, format: String, mediaId: String, recognition: Option[String] = None) extends WechatMessage {
    override val msgType: String = "voice"

    def extraFieldToXMLs: Seq[Elem] = Seq(
      <MediaId>
        {scala.xml.PCData(mediaId)}
      </MediaId>,
      <Format>
        {scala.xml.PCData(format)}
      </Format>
    ) ++ recognition.map(r => <Recognition>{scala.xml.PCData(r)}</Recognition>)

  }

  case class VideoMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, mediaId: String, thumbMediaId: String) extends WechatMessage {
    override val msgType: String = "video"

    def extraFieldToXMLs = Seq(
      <MediaId>
        {scala.xml.PCData(mediaId)}
      </MediaId>,
      <ThumbMediaId>
        {scala.xml.PCData(thumbMediaId)}
      </ThumbMediaId>
    )
  }

  case class ShortVideoMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, mediaId: String, thumbMediaId: String) extends WechatMessage {
    override val msgType: String = "shortvideo"

    def extraFieldToXMLs = Seq(
      <MediaId>
        {scala.xml.PCData(mediaId)}
      </MediaId>,
      <ThumbMediaId>
        {scala.xml.PCData(thumbMediaId)}
      </ThumbMediaId>
    )
  }

  case class LocationMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, locationX: Double, locationY: Double, scale: Double, label: String) extends WechatMessage {
    override val msgType: String = "location"

    def extraFieldToXMLs = Seq(
      <Location_X>
        {locationX}
      </Location_X>,
      <Location_Y>
        {locationY}
      </Location_Y>,
      <Scale>
        {scale}
      </Scale>,
      <Label>
        {scala.xml.PCData(label)}
      </Label>
    )
  }


  case class LinkMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, title: String, description: String, url: String) extends WechatMessage {
    override val msgType: String = "link"

    def extraFieldToXMLs = Seq(
      <Title>
        {scala.xml.PCData(title)}
      </Title>,
      <Description>
        {scala.xml.PCData(description)}
      </Description>,
      <Url>
        {scala.xml.PCData(url)}
      </Url>
    )
  }


  trait WechatEvent extends WechatMessage {
    def event: String
    def extraFieldToXMLs: Seq[Elem] = Seq.empty
    def msgType: String = "event"
  }

  case class SubscribeEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long) extends WechatEvent {
    override def event: String = "subscribe"
  }

  case class UnsubscribeEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long) extends WechatEvent {
    override def event: String = "unsubscribe"
  }


}
