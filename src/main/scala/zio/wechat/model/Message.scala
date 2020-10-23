package zio.wechat.model

import sttp.tapir.DecodeResult
import shapeless.syntax.std.tuple._

import scala.xml.{Elem, XML}

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
          case "scancode_push" =>
            DecodeResult.Value(
              ScanCodePushEvent tupled baseMessage(root) :+ (root \ "EventKey").text :+ (ScanType()) :+ (root \ "ScanResult").text
            )
          case "scancode_waitmsg" =>
            DecodeResult.Value(
              ScanCodeWaitEvent tupled baseMessage(root) :+ (root \ "EventKey").text :+ (ScanType()) :+ (root \ "ScanResult").text
            )
          case e@("pic_sysphoto" | "pic_photo_or_album" | "pic_weixin") =>
            val cons = e match {
              case "pic_sysphoto" => SystemPhotoEvent.tupled
              case "pic_photo_or_album" => AlbumEvent.tupled
              case "pic_weixin" => PicWeChatEvent.tupled
            }
            DecodeResult.Value(
              cons(baseMessage(root) :+ (root \ "EventKey").text :+ ({
                val count = (root \ "SendPicsInfo" \ "Count").text.toInt
                val infos = (root \ "SendPicsInfo" \ "PicList").map(node => {
                  (node \ "item" \ "PicMd5Sum").text
                })
                SendPicsInfo(count, infos)
              }))
            )
          case "location_select" =>
            DecodeResult.Value(
              LocationSelectEvent tupled baseMessage(root) :+ (root \ "EventKey").text :+ {
                val info = root \ "SendLocationInfo"
                val x = (info \ "Location_X").text
                val y = (info \ "Location_Y").text
                val scale = (info \ "Scale").text
                val label = (info \ "Label").text
                val poiName = (info \ "Poiname").text
                SendLocationInfo(x, y, scale, label, poiName)
              })
          case "view_miniprogram" =>
            DecodeResult.Value(
              MiniProgramEvent tupled baseMessage(root) :+ (root \ "EventKey").text :+ (root \ "MenuId").text
            )
        }
      case _ => DecodeResult.Missing
    }
  }
}

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

case class ScanCodePushEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, eventKey: String, scanCodeInfo: ScanType, scanResult: String) extends WechatRequestMessage with WechatEvent {
  override def event: String = "scancode_push"
}

case class ScanCodeWaitEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, eventKey: String, scanCodeInfo: ScanType, scanResult: String) extends WechatRequestMessage with WechatEvent {
  override def event: String = "scancode_waitmsg"
}

case class SystemPhotoEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, eventKey: String, sendPicsInfo: SendPicsInfo) extends WechatRequestMessage with WechatEvent {
  override def event: String = "pic_sysphoto"
}

case class AlbumEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, eventKey: String, sendPicsInfo: SendPicsInfo) extends WechatRequestMessage with WechatEvent {
  override def event: String = "pic_photo_or_album"
}

case class PicWeChatEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, eventKey: String, sendPicsInfo: SendPicsInfo) extends WechatRequestMessage with WechatEvent {
  override def event: String = "pic_weixin"
}

case class LocationSelectEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, eventKey: String, sendLocationInfo: SendLocationInfo) extends WechatRequestMessage with WechatEvent {
  override def event: String = "location_select"
}

case class MiniProgramEvent(toUsername: String, fromUsername: String, createTime: Int, msgId: Long, eventKey: String, menuId: String) extends WechatRequestMessage with WechatEvent {
  override def event: String = "view_miniprogram"
}

case class ScanType() {
  val scanType = "qrcode"
}

case class SendPicsInfo(count: Int, PicList: Seq[String])

case class SendLocationInfo(x: String, y: String, scale: String, label: String, poiName: String)