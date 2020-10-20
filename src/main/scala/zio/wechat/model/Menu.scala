package zio.wechat.model

import enumeratum._

case class MainMenu(button: Seq[Menu])

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


