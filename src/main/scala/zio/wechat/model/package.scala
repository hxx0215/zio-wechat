package zio.wechat

import io.circe.{Decoder, Encoder, HCursor, Json}
import sttp.tapir.{Codec, DecodeResult}

import scala.xml.{Elem, MetaData, NamespaceBinding, Node, XML}
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

    def msgId: Long
  }

  sealed trait WechatRequestMessage extends WechatMessage

  object WechatRequestMessage {
    def baseMessage(root: Elem): (String, String, Int, Long) = {
      (
        (root \ "ToUserName").text,
        (root \ "FromUserName").text,
        (root \ "CreateTime").text.toInt, {
        val text = (root \ "MsgId").text
        // Event do not have msg id
        if (text == "") {
          0L
        } else {
          text.toLong
        }
      }

      )
    }

    def fromString(xmlString: String): DecodeResult[WechatRequestMessage] = {
      val root = XML.loadString(xmlString)
      (root \ "MsgType").text match {
        case "text" => DecodeResult.Value {
          TextRequestMessage.apply _ tupled baseMessage(root) :+ (root \ "Content").text
        }
        case "image" =>
          DecodeResult.Value(
            ImageRequestMessage.apply _ tupled baseMessage(root) ++ ((root \ "PicUrl").text, (root \ "MediaId").text)
          )
        case "voice" =>
          DecodeResult.Value(
            VoiceRequestMessage.apply _ tupled baseMessage(root) ++ ((root \ "Format").text, (root \ "MediaId").text, (root \ "Recognition").headOption.map(_.text))
          )
        case "video" =>
          DecodeResult.Value(
            VideoRequestMessage.apply _ tupled baseMessage(root) ++ ((root \ "MediaId").text, (root \ "ThumbMediaId").text)
          )
        case "shortvideo" =>
          DecodeResult.Value(
            ShortVideoRequestMessage.apply _ tupled baseMessage(root) ++ ((root \ "MediaId").text, (root \ "ThumbMediaId").text)
          )
        case "location" =>
          DecodeResult.Value(
            LocationRequestMessage.apply _ tupled baseMessage(root) ++ ((root \ "Location_X").text.toDouble, (root \ "Location_Y").text.toDouble, (root \ "Scale").text.toDouble, (root \ "Label").text)
          )
        case "link" =>
          DecodeResult.Value(
            LinkRequestMessage.apply _ tupled baseMessage(root) ++ ((root \ "Title").text, (root \ "Description").text, (root \ "Url").text)
          )
        case "event" =>
          (root \ "Event").text match {
            case "subscribe" =>
              DecodeResult.Value(
                SubscribeEvent.apply _ tupled baseMessage(root)
              )
            case "unsubscribe" =>
              DecodeResult.Value(
                UnsubscribeEvent.apply _ tupled baseMessage(root)
              )
          }
        case _ => DecodeResult.Missing
      }
    }
  }

  implicit val wechatRequestMessage: XmlCodec[WechatRequestMessage] = Codec.xml(WechatRequestMessage.fromString) {
    // no used cause you will not response a request message, so no need to encode it to string
    _ => ""
  }

  case class TextRequestMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, content: String) extends WechatRequestMessage {
    override val msgType: String = "text"

    def extraFieldToXMLs = Seq(
      <Content>
        {scala.xml.PCData(content)}
      </Content>
    )
  }

  case class ImageRequestMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, picUrl: String, mediaId: String) extends WechatRequestMessage {
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

  case class VoiceRequestMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, format: String, mediaId: String, recognition: Option[String] = None) extends WechatRequestMessage {
    override val msgType: String = "voice"

    def extraFieldToXMLs: Seq[Elem] = Seq(
      <MediaId>
        {scala.xml.PCData(mediaId)}
      </MediaId>,
      <Format>
        {scala.xml.PCData(format)}
      </Format>
    ) ++ {
      recognition.map(r => <Recognition>
        {scala.xml.PCData(r)}
      </Recognition>).toSeq
    }
  }

  case class VideoRequestMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, mediaId: String, thumbMediaId: String) extends WechatRequestMessage {
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

  case class ShortVideoRequestMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, mediaId: String, thumbMediaId: String) extends WechatRequestMessage {
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

  case class LocationRequestMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, locationX: Double, locationY: Double, scale: Double, label: String) extends WechatRequestMessage {
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


  case class LinkRequestMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, title: String, description: String, url: String) extends WechatRequestMessage {
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

  trait WechatEvent {
    def event: String

    def msgType: String = "event"
  }

  case class SubscribeEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long) extends WechatRequestMessage with WechatEvent {
    override def event: String = "subscribe"
  }

  case class UnsubscribeEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long) extends WechatRequestMessage with WechatEvent {
    override def event: String = "unsubscribe"
  }

  sealed trait WechatResponseMessage extends WechatMessage {

    def toXML: Elem = {
      val base = {
        <xml>
          <ToUserName>
            {scala.xml.PCData(toUsername)}
          </ToUserName>
          <FromUserName>
            {scala.xml.PCData(fromUsername)}
          </FromUserName>
          <CreateTime>
            {createTime}
          </CreateTime>
          <MsgType>
            {scala.xml.PCData(msgType)}
          </MsgType>
          <MsgId>
            {msgId}
          </MsgId>
        </xml>
      }
      base.copy(child = base.child ++ extraFieldToXMLs)
    }

    def extraFieldToXMLs: Seq[Elem]
  }

  case class TextResponseMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, content: String) extends WechatResponseMessage {
    override def extraFieldToXMLs = Seq(
      <Content>
        {scala.xml.PCData(content)}
      </Content>
    )

    override def msgType: String = "text"
  }

  implicit val responseCodec: Codec.XmlCodec[WechatResponseMessage] = Codec.xml(_ => DecodeResult.Missing)(_.toXML.toString())

  case class EmptyResponseMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long) extends WechatResponseMessage {
    override def extraFieldToXMLs: Seq[Elem] = Seq()

    override def toXML: Elem = {
      val empty = <xml/>
      class EmptyElm() extends Elem(empty.prefix, empty.label, empty.attributes, empty.scope, empty.minimizeEmpty, empty.child: _*) {
        override def toString(): String = "success"
      }
      new EmptyElm()
    }

    override def msgType: String = ""
  }


}
