package zio.wechat

import io.circe.{Decoder, Encoder, HCursor, Json}
import sttp.tapir.{Codec, DecodeResult}

import scala.xml.{Elem, XML}
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

  def baseMessage(root: Elem) = {
    (
      (root \ "ToUserName").text,
      (root \ "FromUserName").text,
      (root \ "CreateTime").text.toInt,
      (root \ "MsgId").text.toLong
    )
  }

  def baseXML(m: WechatRequestMessage) = {
    <xml>
      <ToUserName>
        {scala.xml.PCData(m.toUsername)}
      </ToUserName>
      <FromUserName>
        {scala.xml.PCData(m.fromUsername)}
      </FromUserName>
      <CreateTime>
        {m.createTime}
      </CreateTime>
      <MsgType>
        {scala.xml.PCData(m.msgType)}
      </MsgType>
      <MsgId>
        {m.msgId}
      </MsgId>
    </xml>
  }

  sealed trait WechatRequestMessage {
    val toUsername: String
    val fromUsername: String
    val createTime: Int
    val msgType: String
    val msgId: Long
    def toXML: String
  }

  implicit val wechatRequestMessage: XmlCodec[WechatRequestMessage] = Codec.xml(str =>{
    val root =XML.loadString(str)
    val messageType = (root \ "MsgType").text
    (messageType match {
      case "text" => TextRequestMessage.fromString(str)
      case "image" => ImageRequestMessage.fromString(str)
      case "voice" => VoiceRequestMessage.fromString(str)
      case _ => DecodeResult.Missing
    })
  })(_.toXML)

  case class TextRequestMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, content: String) extends WechatRequestMessage {
    override val msgType: String = "text"

    override def toXML: String = {
      val base = baseXML(this)
      val xmlEl = base.copy(child = base.child :+ {
        <Content>
          {scala.xml.PCData(content)}
        </Content>
      })
      xmlEl.toString()
    }
  }

  object TextRequestMessage{
    def fromString(str: String): DecodeResult.Value[TextRequestMessage] = {
      val root = XML.loadString(str)
      val args = baseMessage(root) :+ (root \ "Content").text
      DecodeResult.Value(
        (TextRequestMessage.apply _) tupled args
      )
    }
  }

  case class ImageRequestMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, picUrl: String, mediaId: String) extends WechatRequestMessage {
    override val msgType: String = "image"

    override def toXML: String = {
      val base = baseXML(this)
      val xmlEl = base.copy(child = base.child ++ Seq(
        <PicUrl>
          {scala.xml.PCData(picUrl)}
        </PicUrl>,
        <MediaId>
          {scala.xml.PCData(mediaId)}
        </MediaId>
      ))
      xmlEl.toString()
    }
  }

  object ImageRequestMessage{
    def fromString(str: String): DecodeResult.Value[ImageRequestMessage] = {
      val root = XML.loadString(str)
      val args = baseMessage(root) ++ ((root \ "PicUrl").text, (root \ "MediaId").text)
      DecodeResult.Value(
        (ImageRequestMessage.apply _) tupled args
      )
    }
  }

  case class VoiceRequestMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, format: String, mediaId: String, recognition: Option[String] = None) extends WechatRequestMessage {
    override val msgType: String = "voice"

    override def toXML: String = {
      val base = baseXML(this)
      val xmlEl = base.copy(child = base.child ++ Seq(
        <MediaId>
          {scala.xml.PCData(mediaId)}
        </MediaId>,
        <Format>
          {scala.xml.PCData(format)}
        </Format>
      ) ++ {
        recognition.map(r => <Recognition>{scala.xml.PCData(r)}</Recognition>).toSeq
      })
      xmlEl.toString()
    }
  }
  object VoiceRequestMessage{
    def fromString(str: String): DecodeResult.Value[VoiceRequestMessage] = {
      val root = XML.loadString(str)
      val args = baseMessage(root) ++ ((root \ "Format").text, (root \ "MediaId").text, (root \ "Recognition").headOption.map(_.text))
      DecodeResult.Value(
        (VoiceRequestMessage.apply _) tupled args
      )
    }
  }


    case class VideoRequestMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, mediaId: String, thumbMediaId: String) extends WechatRequestMessage {
      override val msgType: String = "video"

      override def toXML: String = {
        val base = baseXML(this)
        val xmlEL = base.copy(child = base.child ++ Seq(
          <MediaId>{scala.xml.PCData(mediaId)}</MediaId>,
          <ThumbMediaId>{scala.xml.PCData(thumbMediaId)}</ThumbMediaId>
        ))
        xmlEL.toString()


      }
    }

  //  case class ShortVideoMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, mediaId: String, thumbMediaId: String) extends WechatMessage {
  //    override val msgType: String = "shortvideo"
  //  }
  //
  //  case class LocationMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, locationX: Double, locationY: Double, scale: Double, label: String) extends WechatMessage {
  //    override val msgType: String = "location"
  //  }
  //
  //  case class LinkMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, title: String, description: String, url: String) extends WechatMessage {
  //    override val msgType: String = "link"
  //  }

  trait WechatResponseMessage
  implicit val responseCodec: Codec.XmlCodec[WechatResponseMessage] = Codec.xml(str => ???)(m => ???)

}
