package zio.wechat


import cats.Show
import cats.implicits._
import enumeratum._
import io.circe.{Decoder, Encoder, HCursor, Json}
import shapeless.syntax.std.tuple._
import sttp.tapir.Codec.XmlCodec
import sttp.tapir.{Codec, DecodeResult}
import io.circe.generic.auto._
import io.circe.syntax._

import scala.xml._


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

  sealed trait WechatResponseMessage extends WechatMessage {
    def extraFieldToXMLs: Seq[Elem]
  }

  case object EmptyMessage extends WechatResponseMessage {
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

    def fromString(xmlString: String): DecodeResult[WechatRequestMessage] = {
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
            VoiceRequestMessage tupled baseMessage(root) :+ (root \ "Format").text :+ (root \ "MediaId").text :+ (root \ "Recognition").headOption.map(_.text)
          )
        case "video" =>
          DecodeResult.Value(
            VideoRequestMessage tupled baseMessage(root) :+ (root \ "MediaId").text :+ (root \ "ThumbMediaId").text
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
                SubscribeEvent tupled baseMessage(root) :+ (root \ "EventKey").text :+ (root \ "Ticket").text
              )
            case "unsubscribe" =>
              DecodeResult.Value(
                UnsubscribeEvent tupled baseMessage(root)
              )
            case "SCAN" =>
              DecodeResult.Value(
                ScanEvent tupled baseMessage(root) :+ (root \ "EventKey").text :+ (root \ "Ticket").text
              )
            case "LOCATION" =>
              DecodeResult.Value(
                LocationEvent tupled baseMessage(root) :+ (root \ "Latitude").text.toDouble :+ (root \ "Longitude").text.toDouble :+ (root \ "Precision").text.toDouble
              )
            case "CLICK" =>
              DecodeResult.Value(
                ClickEvent tupled baseMessage(root) :+ (root \ "EventKey").text
              )
            case "VIEW" =>
              DecodeResult.Value(
                ViewEvent tupled baseMessage(root) :+ (root \ "EventKey").text
              )
          }
        case _ => DecodeResult.Missing
      }
    }
  }

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

  case class TextMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, content: String) extends WechatRequestMessage with WechatResponseMessage {
    override val msgType: String = "text"

    override def extraFieldToXMLs = Seq(
      <Content>
        {scala.xml.PCData(content)}
      </Content>
    )
  }

  case class ImageMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, picUrl: String, mediaId: String) extends WechatRequestMessage with WechatResponseMessage {
    override val msgType: String = "image"

    def extraFieldToXMLs = Seq(
      <Image>
        <MediaId>
          {scala.xml.PCData(mediaId)}
        </MediaId>
      </Image>
    )
  }

  case class VoiceRequestMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, format: String, mediaId: String, recognition: Option[String] = None) extends WechatRequestMessage {
    override val msgType: String = "voice"

    //    def extraFieldToXMLs: Seq[Elem] = Seq(
    //      <MediaId>
    //        {scala.xml.PCData(mediaId)}
    //      </MediaId>,
    //      <Format>
    //        {scala.xml.PCData(format)}
    //      </Format>
    //    ) ++ recognition.map(r => <Recognition>
    //      {scala.xml.PCData(r)}
    //    </Recognition>)
  }

  case class VoiceResponseMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, mediaId: String) extends WechatResponseMessage {
    override def extraFieldToXMLs: Seq[Elem] = Seq(
      <Voice>
        <MediaId>
          {scala.xml.PCData(mediaId)}
        </MediaId>
      </Voice>
    )

    override def msgType: String = "voice"
  }

  case class VideoRequestMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, mediaId: String, thumbMediaId: String) extends WechatRequestMessage {
    override val msgType: String = "video"

    //    def extraFieldToXMLs = Seq(
    //      <MediaId>
    //        {scala.xml.PCData(mediaId)}
    //      </MediaId>,
    //      <ThumbMediaId>
    //        {scala.xml.PCData(thumbMediaId)}
    //      </ThumbMediaId>
    //    )
  }

  case class VideoResponseMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, mediaId: String, title: String, description: String) extends WechatResponseMessage {
    override def extraFieldToXMLs: Seq[Elem] = Seq(
      <Video>
        <MediaId>
          {scala.xml.PCData(mediaId)}
        </MediaId>
        <Title>
          {scala.xml.PCData(title)}
        </Title>
        <Description>
          {scala.xml.PCData(description)}
        </Description>
      </Video>
    )

    override def msgType: String = "video"
  }

  case class ShortVideoMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, mediaId: String, thumbMediaId: String) extends WechatRequestMessage {
    override val msgType: String = "shortvideo"

    //    def extraFieldToXMLs = Seq(
    //      <MediaId>
    //        {scala.xml.PCData(mediaId)}
    //      </MediaId>,
    //      <ThumbMediaId>
    //        {scala.xml.PCData(thumbMediaId)}
    //      </ThumbMediaId>
    //    )
  }

  case class LocationMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, locationX: Double, locationY: Double, scale: Double, label: String) extends WechatRequestMessage {
    override val msgType: String = "location"

    //    def extraFieldToXMLs = Seq(
    //      <Location_X>
    //        {locationX}
    //      </Location_X>,
    //      <Location_Y>
    //        {locationY}
    //      </Location_Y>,
    //      <Scale>
    //        {scale}
    //      </Scale>,
    //      <Label>
    //        {scala.xml.PCData(label)}
    //      </Label>
    //    )
  }


  case class LinkMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, title: String, description: String, url: String) extends WechatRequestMessage {
    override val msgType: String = "link"

    //    def extraFieldToXMLs = Seq(
    //      <Title>
    //        {scala.xml.PCData(title)}
    //      </Title>,
    //      <Description>
    //        {scala.xml.PCData(description)}
    //      </Description>,
    //      <Url>
    //        {scala.xml.PCData(url)}
    //      </Url>
    //    )
  }

  case class MusicResponseMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, title: String, description: String, musicURL: String, hqMusicURL: String, thumbMediaId: String) extends WechatResponseMessage {

    import scala.xml.PCData

    override def extraFieldToXMLs: Seq[Elem] = Seq(
      <Music>
        <Title>
          {PCData(title)}
        </Title>
        <Description>
          {PCData(description)}
        </Description>
        <MusicUrl>
          {PCData(musicURL)}
        </MusicUrl>
        <HQMusicUrl>
          {PCData(hqMusicURL)}
        </HQMusicUrl>
        <ThumbMediaId>
          {PCData(thumbMediaId)}
        </ThumbMediaId>
      </Music>
    )

    override def msgType: String = "music"
  }

  case class NewsResponseMessage(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, articleCount: Int, articles: Seq[NewsArticle]) extends WechatResponseMessage {
    override def extraFieldToXMLs: Seq[Elem] = Seq(
      <ArticleCount>
        {articleCount}
      </ArticleCount>,
      <Articles>
        {articles.map(_.toXML)}
      </Articles>
    )

    override def msgType: String = "news"
  }

  case class NewsArticle(title: String, description: String, picURL: String, url: String) {

    import scala.xml.PCData

    def toXML: Elem = {
      <item>
        <Title>
          {PCData(title)}
        </Title>
        ,
        <Description>
          {PCData(description)}
        </Description>
        <PicUrl>
          {PCData(picURL)}
        </PicUrl>
        <Url>
          {PCData(url)}
        </Url>
      </item>
    }
  }

  trait WechatEvent {
    def event: String

    def extraFieldToXMLs: Seq[Elem] = Seq.empty

    def msgType: String = "event"
  }

  case class SubscribeEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, eventKey: String, ticket: String) extends WechatRequestMessage with WechatEvent {
    override def event: String = "subscribe"
  }

  case class UnsubscribeEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long) extends WechatRequestMessage with WechatEvent {
    override def event: String = "unsubscribe"
  }

  case class ScanEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, eventKey: String, ticket: String) extends WechatRequestMessage with WechatEvent {
    override def event: String = "SCAN"
  }

  case class LocationEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, latitude: Double, longitude: Double, precision: Double) extends WechatRequestMessage with WechatEvent {
    override def event: String = "LOCATION"
  }

  //自定义菜单取消息
  case class ClickEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, eventKey: String) extends WechatRequestMessage with WechatEvent {
    override def event: String = "CLICK"
  }

  case class ViewEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, eventKey: String) extends WechatRequestMessage with WechatEvent {
    override def event: String = "VIEW"
  }

  sealed abstract class QRCodeActionName(override val entryName: String) extends EnumEntry

  object QRCodeActionName extends Enum[QRCodeActionName] {
    val values = findValues

    case object QRSCENE extends QRCodeActionName("QR_SCENE")
    case object QRLIMITSTRSCENE extends QRCodeActionName("QR_LIMIT_STR_SCENE")
  }

  import QRCodeActionName._

  case class TemporaryQRCodeRequest(expireSeconds: Int, actionName: QRCodeActionName, actionInfo: QRCodeActionInfo)

  implicit val temporaryQRCodeRequestEncoder: Encoder[TemporaryQRCodeRequest] = (r: TemporaryQRCodeRequest) => Json.obj(
    "expire_seconds" -> Json.fromInt(r.expireSeconds),
    "action_name" -> Json.fromString(r.actionName.toString),
    "action_info" -> r.actionInfo.asJson
  )
  implicit val temporaryQRCodeRequestDecoder: Decoder[TemporaryQRCodeRequest] = (c: HCursor) => for {
    expireSeconds <- c.downField("expire_seconds").as[Int]
    actionName <- c.downField("action_name").as[String]
    actionInfo <- c.downField("action_info").as[QRCodeActionInfo]
  } yield TemporaryQRCodeRequest(expireSeconds, QRCodeActionName.withName(actionName), actionInfo)

  case class QRCodeActionInfo(scene: QRCodeScene)

  sealed trait QRCodeScene

  case class QRCodeIdScene(scene: Int) extends QRCodeScene

  implicit val qrCodeIdSceneEncoder: Encoder[QRCodeIdScene] = (idScene: QRCodeIdScene) => Json.obj(
    "scene_id" -> Json.fromInt(idScene.scene)
  )

  case class QRCodeStringScene(scene: String) extends QRCodeScene

  implicit val qrCodeStringSceneEncoder: Encoder[QRCodeStringScene] = (stringScene: QRCodeStringScene) => Json.obj(
    "scene_str" -> Json.fromString(stringScene.scene)
  )

  case class QRCodeResponse(ticket: String, expireSeconds: Int, url: String)

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
