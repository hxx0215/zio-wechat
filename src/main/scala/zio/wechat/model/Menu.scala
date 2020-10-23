package zio.wechat.model

import enumeratum._
import io.circe._
import io.circe.generic.JsonCodec
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

  case object News extends MenuType("news")

}

sealed trait Menu {
  def name: String
}

object Menu {
  implicit val menuDecoder: Decoder[Menu] = (c: HCursor) => for {
    name <- c.downField("name").as[String]
    t <- c.downField("type").as[Option[String]]
    value <- c.downField("value").as[Option[String]]
    subButton <- c.downField("sub_button").as[Option[SubButtonList]]
    key <- c.downField("key").as[Option[String]]
    url <- c.downField("url").as[Option[String]]
    mediaId <- c.downField("media_id").as[Option[String]]
    appId <- c.downField("app_id").as[Option[String]]
    pagePath <- c.downField("pagepath").as[Option[String]]
    newsInfo <- c.downField("news_info").as[Option[NewsInfos]]
  } yield {
    if (t.isDefined) {
      if (value.isDefined) {
        NewsMenu(name, value.get, newsInfo.get)
      } else {
        ConcreteMenu(name, MenuType.withName(t.get), key, url, mediaId, appId, pagePath)
      }
    } else {
      SubButtonMenu(name, subButton.get)
    }
  }
  implicit val menuEncoder: Encoder[Menu] = Encoder.instance({
    case sub: SubMenu => sub.asJson
    case concreteMenu: ConcreteMenu => concreteMenu.asJson
    case news: NewsMenu => news.asJson
    case subMenu: SubButtonMenu => subMenu.asJson
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

@ConfiguredJsonCodec case class NewsInfo(title: String, author: String, digest: String, showCover: Int, coverUrl: String, contentUrl: String, sourceUrl: String)

object NewsInfo {
  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
}

@JsonCodec case class NewsInfos(list: Seq[NewsInfo])

@ConfiguredJsonCodec case class NewsMenu(name: String, value: String, newsInfo: NewsInfos) extends Menu {
  val `type` = "news"
}

object NewsMenu {
  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
}

@ConfiguredJsonCodec case class MenuInformation(isMenuOpen: Int, selfmenuInfo: MainMenu)

object MenuInformation {
  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
}


@JsonCodec case class SubButtonList(list: Seq[Menu])

@ConfiguredJsonCodec case class SubButtonMenu(name: String, subButton: SubButtonList) extends Menu

object SubButtonMenu {
  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
}


case class MatchRule(tagId: Option[String] = None,
                     sex: Option[String] = None,
                     country: Option[String] = None,
                     province: Option[String] = None,
                     city: Option[String] = None,
                     clientPlatformType: Option[String] = None,
                     language: Option[String] = None)

object MatchRule {
  implicit val configuration: Configuration = Configuration.default.withSnakeCaseMemberNames
  implicit val matchRuleEncoder: Encoder[MatchRule] = deriveEncoder[MatchRule].mapJson(_.dropNullValues)
  implicit val matchRuleDecoder: Decoder[MatchRule] = deriveDecoder[MatchRule]
}

@ConfiguredJsonCodec case class CustomMainMenu(button: Seq[Menu], matchRule: MatchRule)

object CustomMainMenu {
  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
}