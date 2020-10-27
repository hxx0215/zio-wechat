package zio.wechat.model

import scala.collection.immutable.Seq
import java.io.File

import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._
import sttp.model.Part

case class UpdateCustomSupportMessage(account: String, nickname: String, password: String)

object UpdateCustomSupportMessage {
  implicit val encoder: Encoder[UpdateCustomSupportMessage] = Encoder.forProduct3("kf_account", "nickname", "password")(UpdateCustomSupportMessage.unapply(_).get)
  implicit val decoder: Decoder[UpdateCustomSupportMessage] = Decoder.forProduct3("kf_account", "nickname", "password")(UpdateCustomSupportMessage.apply)
}

case class CustomSupportAccount(account: String)

object CustomSupportAccount {
  implicit val encoder: Encoder[CustomSupportAccount] = Encoder.forProduct1("kf_account")(CustomSupportAccount.unapply(_).get)
  implicit val decoder: Decoder[CustomSupportAccount] = Decoder.forProduct1("kf_account")(CustomSupportAccount.apply)
}

case class CustomSupportAvatarForm(media: Part[File])

case class CustomSupportList(list: Seq[CustomSupportInfo])

object CustomSupportList {
  implicit val encoder: Encoder[CustomSupportList] = Encoder.instance {
    case CustomSupportList(list) => Json.obj(
      "kf_list" -> list.asJson
    )
  }
  implicit val decoder: Decoder[CustomSupportList] = Decoder.instance[CustomSupportList](c =>
    c.get[Seq[CustomSupportInfo]]("kf_list").map(CustomSupportList.apply)
  )
}


case class CustomSupportInfo(account: String, nick: String, id: String, imageURL: String)

object CustomSupportInfo {
  implicit val encoder: Encoder[CustomSupportInfo] = Encoder.forProduct4("kf_account", "kf_nick", "kf_id", "kf_headimgurl")(CustomSupportInfo.unapply(_).get)
  implicit val decoder: Decoder[CustomSupportInfo] = Decoder.forProduct4("kf_account", "kf_nick", "kf_id", "kf_headimgurl")(CustomSupportInfo.apply)
}


sealed trait CustomMessage {
  def to: String

  def msgType: String

  def customServiceInfo: Option[CustomSupportAccount]
}

case class CustomTextMessage(to: String, text: CustomTextMessageContent, customServiceInfo: Option[CustomSupportAccount] = None) extends CustomMessage {
  override def msgType: String = "text"
}

case class CustomTextMessageContent(content: String)

case class CustomImageMessage(to: String, image: CustomMediaMessageContent, customServiceInfo: Option[CustomSupportAccount] = None) extends CustomMessage {
  override def msgType: String = "image"
}

case class CustomMediaMessageContent(mediaId: String)

object CustomMediaMessageContent {

  import io.circe.derivation._

  implicit val encoder: Encoder.AsObject[CustomMediaMessageContent] = deriveEncoder(derivation.renaming.snakeCase)
  implicit val decoder: Decoder[CustomMediaMessageContent] = deriveDecoder(derivation.renaming.snakeCase)
}

case class CustomVoiceMessage(to: String, voice: CustomMediaMessageContent, customServiceInfo: Option[CustomSupportAccount] = None) extends CustomMessage {
  override def msgType: String = "voice"
}

case class CustomVideoMessage(to: String, video: CustomVideoMessageContent, customServiceInfo: Option[CustomSupportAccount] = None) extends CustomMessage {
  override def msgType: String = "video"
}

case class CustomVideoMessageContent(mediaId: String, thumbMediaId: String, title: String, description: String)

object CustomVideoMessageContent {

  import io.circe.derivation._

  implicit val encoder: Encoder.AsObject[CustomVideoMessageContent] = deriveEncoder(derivation.renaming.snakeCase)
  implicit val decoder: Decoder[CustomMediaMessageContent] = deriveDecoder(derivation.renaming.snakeCase)
}

case class CustomMusicMessage(to: String, music: CustomMusicMessageContent, customServiceInfo: Option[CustomSupportAccount] = None) extends CustomMessage {
  override def msgType: String = "music"
}

case class CustomMusicMessageContent(title: String, description: String, musicUrl: String, hqMusicUrl: String, thumbMediaId: String)

object CustomMusicMessageContent {
  implicit val encoder: Encoder[CustomMusicMessageContent] = Encoder.forProduct5("title",
    "description",
    "musicurl",
    "hqmusicurl",
    "thumb_media_id")(CustomMusicMessageContent.unapply(_).get)
  implicit val decoder: Decoder[CustomMusicMessageContent] = Decoder.forProduct5("title",
    "description",
    "musicurl",
    "hqmusicurl",
    "thumb_media_id")(CustomMusicMessageContent.apply)
}

case class CustomInternetNewsMessage(to: String, news: CustomNewsArticle, customServiceInfo: Option[CustomSupportAccount] = None) extends CustomMessage {
  override def msgType: String = "news"
}

case class CustomNewsArticle(articles: Seq[CustomInternetNews])

object CustomNewsArticle {
  implicit val encoder: Encoder[CustomNewsArticle] = Encoder.instance {
    case CustomNewsArticle(articles) => Json.obj(
      "articles" -> articles.asJson
    )
  }

  implicit val decoder: Decoder[CustomNewsArticle] = Decoder.instance { c =>
    c.get[Seq[CustomInternetNews]]("articles").map(CustomNewsArticle.apply)
  }
}

case class CustomInternetNews(title: String, description: String, url: String, picUrl: String)

object CustomInternetNews {
  implicit val encoder: Encoder[CustomInternetNews] = Encoder.forProduct4("title",
    "description",
    "url",
    "picurl")(CustomInternetNews.unapply(_).get)
  implicit val decoder: Decoder[CustomInternetNews] = Decoder.forProduct4("title",
    "description",
    "url",
    "picurl")(CustomInternetNews.apply)
}

case class CustomMPNewsMessage(to: String, mpNews: CustomMediaMessageContent, customServiceInfo: Option[CustomSupportAccount] = None) extends CustomMessage {
  override def msgType: String = "mpnews"
}

case class CustomMenuMessage(to: String, msgMenu: CustomMenuContent, customServiceInfo: Option[CustomSupportAccount] = None) extends CustomMessage {
  override def msgType: String = "msgmenu"
}

case class CustomMenuContent(headContent: String, list: Seq[CustomMenuItem], tailContent: String)

object CustomMenuContent {

  import io.circe.derivation._

  implicit val encoder: Encoder.AsObject[CustomMenuContent] = deriveEncoder(derivation.renaming.snakeCase)
  implicit val decoder: Decoder[CustomMenuContent] = deriveDecoder(derivation.renaming.snakeCase)
}

case class CustomMenuItem(id: String, content: String)

object CustomMenuItem {
  implicit val encoder: Encoder[CustomMenuItem] = deriveEncoder
  implicit val decoder: Decoder[CustomMenuItem] = deriveDecoder
}


case class CustomCardMessage(to: String, card: CustomCardContent, customServiceInfo: Option[CustomSupportAccount] = None) extends CustomMessage {
  override def msgType: String = "wxcard"
}

case class CustomCardContent(cardId: String)

object CustomCardContent {

  import io.circe.derivation._

  implicit val encoder: Encoder.AsObject[CustomMenuContent] = deriveEncoder(derivation.renaming.snakeCase)
  implicit val decoder: Decoder[CustomMenuContent] = deriveDecoder(derivation.renaming.snakeCase)
}

case class CustomMiniProgramMessage()

case class CustomMiniProgramContent(title: String, appId: String, pagePath: String, thumbMediaId: String)

object CustomMiniProgramContent {
  implicit val encoder: Encoder[CustomMiniProgramContent] = Encoder.forProduct4("title",
    "appid",
    "pagepath",
    "thumb_media_id")(CustomMiniProgramContent.unapply(_).get)

  implicit val decoder: Decoder[CustomMiniProgramContent] = Decoder.forProduct4("title",
    "appid",
    "pagepath",
    "thum_media_id")(CustomMiniProgramContent.apply)
}