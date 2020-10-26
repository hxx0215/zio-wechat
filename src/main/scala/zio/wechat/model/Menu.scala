package zio.wechat.model

import scala.collection.immutable.Seq
import enumeratum._
import io.circe._
import io.circe.syntax._


sealed abstract class MenuType(override val entryName: String) extends EnumEntry

case object MenuType extends Enum[MenuType] with CirceEnum[MenuType] {
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

  import cats.syntax.functor._

  implicit val menuDecoder: Decoder[Menu] = List[Decoder[Menu]](
    Decoder[MenuGroup].widen,
    Decoder[MenuItem].widen,
    Decoder[NewsMenuItem].widen,
    Decoder[MenuGroupResponse].widen)
    .reduceLeft(_ or _)
  implicit val menuEncoder: Encoder[Menu] = Encoder.instance({
    case sub: MenuGroup => sub.asJson
    case concreteMenu: MenuItem => concreteMenu.asJson
    case news: NewsMenuItem => news.asJson
    case subMenu: MenuGroupResponse => subMenu.asJson
  })
}

case class MenuGroup(name: String, subButton: Seq[Menu]) extends Menu

object MenuGroup {

  import io.circe.derivation._

  implicit val decode: Decoder[MenuGroup] = deriveDecoder(derivation.renaming.snakeCase)
  //    (c: HCursor) => for {
  //    name <- c.downField("name").as[String]
  //    sub <- c.downField("sub_button").as[Seq[Menu]]
  //  } yield MenuGroup(name, sub)

  implicit val encode: Encoder[MenuGroup] = deriveEncoder(derivation.renaming.snakeCase)
  //    Encoder.instance[MenuGroup] {
  //    case MenuGroup(name, subButton) => Json.obj(
  //      "name" -> name.asJson,
  //      "sub_button" -> subButton.asJson
  //    )
  //  }
  //Encoder.forProduct2("name", "sub_button")(m => (m.name, m.subButton))
}

case class MenuItem(name: String, `type`: MenuType, key: Option[String], url: Option[String] = None, mediaId: Option[String] = None, appId: Option[String] = None, pagePath: Option[String] = None) extends Menu

object MenuItem {
  implicit val concreteMenuDecoder: Decoder[MenuItem] =
    Decoder.forProduct7("name",
      "type",
      "key",
      "url",
      "media_id",
      "app_id",
      "pagepath")(MenuItem.apply)

  implicit val encoder: Encoder[MenuItem] =
    (Encoder.forProduct7[MenuItem, String, MenuType, Option[String], Option[String], Option[String], Option[String], Option[String]]("name",
      "type",
      "key",
      "url",
      "media_id",
      "app_id",
      "pagepath")(MenuItem.unapply(_).get)).mapJson(_.dropNullValues)


}

case class NewsInfo(title: String, author: String, digest: String, showCover: Int, coverUrl: String, contentUrl: String, sourceUrl: String)

object NewsInfo {
  implicit val encoder: Encoder[NewsInfo] = Encoder.forProduct7("title",
    "author",
    "digest",
    "show_cover",
    "cover_url",
    "content_url",
    "source_url")(NewsInfo.unapply(_).get)
  implicit val decoder: Decoder[NewsInfo] = Decoder.forProduct7("title",
    "author",
    "digest",
    "show_cover",
    "cover_url",
    "content_url",
    "source_url")(NewsInfo.apply)
}

case class NewsInfos(list: Seq[NewsInfo])

object NewsInfos {
  implicit val encoder: Encoder[NewsInfos] = Encoder.instance[NewsInfos] {
    case NewsInfos(list) => Json.obj(
      "list" -> list.asJson
    )
  }
  implicit val decoder: Decoder[NewsInfos] = Decoder.instance[NewsInfos] { c =>
    c.get[Seq[NewsInfo]]("list").map(NewsInfos.apply)
  }
}

case class NewsMenuItem(name: String, value: String, newsInfo: NewsInfos) extends Menu {
  val `type` = "news"
}

object NewsMenuItem {
  implicit val encoder: Encoder[NewsMenuItem] = Encoder.forProduct4("name", "value", "news_info", "type")(m => {
    (m.name, m.value, m.newsInfo, m.`type`)
  })
  implicit val decoder: Decoder[NewsMenuItem] = Decoder.forProduct3("name", "value", "news_info")(NewsMenuItem.apply)
}

case class MenuInformation(isMenuOpen: Int, selfMenuInfo: MainMenu)

object MenuInformation {
  implicit val encoder: Encoder[MenuInformation] = Encoder.forProduct2("is_menu_open", "selfmenu_info")(MenuInformation.unapply(_).get)
  implicit val decoder: Decoder[MenuInformation] = Decoder.forProduct2("is_menu_open", "selfmenu_info")(MenuInformation.apply)
}


case class SubButtonList(list: Seq[Menu])

object SubButtonList {
  implicit val encoder: Encoder[SubButtonList] = Encoder.instance {
    case SubButtonList(list) => Json.obj(
      "list" -> list.asJson
    )
  }

  implicit val decoder: Decoder[SubButtonList] = Decoder.instance[SubButtonList](c => c.get[Seq[Menu]]("list").map(SubButtonList.apply))
}

/**
 * this class is only used for [[zio.wechat.endpoints.fetchMenuEndpoint]]'s response
 *
 * @param name      name
 * @param subButton sub_button
 */
case class MenuGroupResponse(name: String, subButton: SubButtonList) extends Menu

object MenuGroupResponse {
  implicit val encoder: Encoder[MenuGroupResponse] = Encoder.forProduct2("name", "sub_button")(MenuGroupResponse.unapply(_).get)
  implicit val decoder: Decoder[MenuGroupResponse] = Decoder.forProduct2("name", "sub_button")(MenuGroupResponse.apply)
}


case class MatchRule(tagId: Option[String] = None,
                     sex: Option[String] = None,
                     country: Option[String] = None,
                     province: Option[String] = None,
                     city: Option[String] = None,
                     clientPlatformType: Option[String] = None,
                     language: Option[String] = None)

object MatchRule {

  import io.circe.derivation._

  implicit val matchRuleEncoder: Encoder[MatchRule] = deriveEncoder[MatchRule](derivation.renaming.snakeCase).mapJson(_.dropNullValues)
  implicit val matchRuleDecoder: Decoder[MatchRule] = deriveDecoder[MatchRule]
}

case class MainMenu(button: Seq[Menu], matchRule: Option[MatchRule] = None, menuId: Option[String] = None)

object MainMenu {
  implicit val mainMenuDecoder: Decoder[MainMenu] = Decoder.forProduct3("button", "matchRule", "menuid")(MainMenu.apply)
  implicit val mainMenuEncoder: Encoder[MainMenu] = Encoder.forProduct3[MainMenu, Seq[Menu], Option[MatchRule], Option[String]]("button", "matchRule", "menuid")(MainMenu.unapply(_).get).mapJson(_.dropNullValues)
}

case class MenuId(menuId: String)

object MenuId {
  implicit val encoder: Encoder[MenuId] = Encoder.forProduct1("menuid")(MenuId.unapply(_).get)
  implicit val decoder: Decoder[MenuId] = Decoder.forProduct1("menuid")(MenuId.apply)
}


case class CustomMenuConfig(menu: MainMenu, conditionalMenu: Option[MainMenu])

object CustomMenuConfig {

  import cats.instances.either._
  import cats.implicits.catsSyntaxTuple2Semigroupal

  implicit val encoder: Encoder[CustomMenuConfig] = Encoder.instance {
    case CustomMenuConfig(menu, conditionalMenu) => Json.obj(
      "menu" -> menu.asJson,
      "conditionalmenu" -> conditionalMenu.asJson
    )
  }

  implicit val decoder: Decoder[CustomMenuConfig] = Decoder.instance(c => (
    c.get[MainMenu]("menu"),
    c.get[Option[MainMenu]]("conditionalmenu")
    ).mapN(CustomMenuConfig.apply))
}

