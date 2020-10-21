package zio.wechat.model

import enumeratum._
import io.circe._
import io.circe.generic.extras.{Configuration, ConfiguredJsonCodec, JsonKey}
import io.circe.generic.semiauto._
import io.circe.syntax.EncoderOps

case class MainMenu(button: Seq[Menu])

object MainMenu {
  implicit val mainMenuDecoder: Decoder[MainMenu] = deriveDecoder[MainMenu]
  implicit val mainMenuEncoder: Encoder[MainMenu] = (m: MainMenu) => Json.obj(
    "button" -> m.button.asJson
  )
}

sealed abstract class MenuType(override val entryName: String) extends EnumEntry

case object MenuType extends Enum[MenuType] {
  val values = findValues

  case object Click extends MenuType("click")

  case object View extends MenuType("view")

  case object ScanPush extends MenuType("scancode_push")

  case object ScanWait extends MenuType("scancode_waitmsg")

  case object TakePhoto extends MenuType("pic_sysphoto")

  case object PhotoAlbum extends MenuType("pic_photo_or_album")

  case object WeixinAlbum extends MenuType("pic_weixin")

  case object Location extends MenuType("location_select")

  case object Media extends MenuType("media_id")

  case object ViewLimited extends MenuType("view_limited")

  case object Mini extends MenuType("miniprogram")

}

sealed trait Menu {
  def name: String
}

object Menu {
  implicit val menuDecoder: Decoder[Menu] = deriveDecoder
  implicit val menuEncoder: Encoder[Menu] = Encoder.instance({
    case sub: SubMenu => sub.asJson
    case concreteMenu: ConcreteMenu => concreteMenu.asJson
  })
}

case class SubMenu(name: String, subButton: Seq[Menu]) extends Menu

object SubMenu {

  implicit val subMenuDecoder: Decoder[SubMenu] = (c: HCursor) => for {
    name <- c.downField("name").as[String]
    sub <- c.downField("sub_button").as[Seq[Menu]]
  } yield SubMenu(name, sub)

  implicit val subMenuEncoder: Encoder[SubMenu] = (s: SubMenu) => Json.obj(
    "name" -> Json.fromString(s.name),
    "sub_button" -> s.subButton.asJson
  )
}

case class ConcreteMenu(name: String, `type`: MenuType, key: Option[String], url: Option[String] = None, mediaId: Option[String] = None, appId: Option[String] = None, pagePath: Option[String] = None) extends Menu

object ConcreteMenu {
  implicit val concreteMenuDecoder: Decoder[ConcreteMenu] = (c: HCursor) => for {
    name <- c.downField("name").as[String]
    t <- c.downField("type").as[String]
    key <- c.downField("key").as[Option[String]]
    url <- c.downField("url").as[Option[String]]
    mediaId <- c.downField("media_id").as[Option[String]]
    appId <- c.downField("app_id").as[Option[String]]
    pagePath <- c.downField("pagepath").as[Option[String]]
  } yield
    ConcreteMenu(name, MenuType.withName(t), key, url, mediaId, appId, pagePath)

  implicit val concreteMenuEncoder: Encoder[ConcreteMenu] = new Encoder[ConcreteMenu] {
    override def apply(m: ConcreteMenu): Json = Json.obj(
      ("name", Json.fromString(m.name)),
      ("type", Json.fromString(m.`type`.entryName)),
      ("key", m.key.map(Json.fromString).getOrElse(Json.Null)),
      ("url", m.url.map(Json.fromString).getOrElse(Json.Null)),
      ("media_id", m.mediaId.map(Json.fromString).getOrElse(Json.Null)),
      ("app_id", m.appId.map(Json.fromString).getOrElse(Json.Null)),
      ("pagepath", m.pagePath.map(Json.fromString).getOrElse(Json.Null))
    )
  }.mapJson(_.dropNullValues)


}

