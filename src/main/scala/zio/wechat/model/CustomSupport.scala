package zio.wechat.model

import scala.collection.immutable.Seq
import java.io.File

import io.circe._
import io.circe.generic.JsonCodec
import io.circe.syntax._
import io.circe.generic.semiauto._
import cats.implicits._
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

object CustomTextMessage {
  implicit val encoder: Encoder[CustomTextMessage] = (Encoder.instance[CustomTextMessage] {
    case m@CustomTextMessage(to, text, customServiceInfo) => Json.obj(
      "touser" -> to.asJson,
      "text" -> text.asJson,
      "msgtype" -> m.msgType.asJson,
      "customservice" -> customServiceInfo.asJson
    )
  }).mapJson(_.dropNullValues)

  implicit val decoder: Decoder[CustomTextMessage] = Decoder.instance(c => (
    c.get[String]("touser"),
    c.get[CustomTextMessageContent]("text"),
    c.get[Option[CustomSupportAccount]]("customservice")
    ).mapN(CustomTextMessage.apply))
}

@JsonCodec case class CustomTextMessageContent(content: String)

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

object CustomVoiceMessage {
  implicit val encoder: Encoder[CustomVoiceMessage] = Encoder.instance[CustomVoiceMessage] {
    case m@CustomVoiceMessage(to, voice, customServiceInfo) => Json.obj(
      "touser" -> to.asJson,
      "msgtype" -> m.msgType.asJson,
      "voice" -> voice.asJson,
      "customservice" -> customServiceInfo.asJson
    )
  } mapJson (_.dropNullValues)

  implicit val decoder: Decoder[CustomVoiceMessage] = Decoder.instance[CustomVoiceMessage] { c =>
    (c.get[String]("touser"),
      c.get[CustomMediaMessageContent]("voice"),
      c.get[Option[CustomSupportAccount]]("customservice")).mapN(CustomVoiceMessage.apply)
  }
}

case class CustomVideoMessage(to: String, video: CustomVideoMessageContent, customServiceInfo: Option[CustomSupportAccount] = None) extends CustomMessage {
  override def msgType: String = "video"
}

object CustomVideoMessage {
  implicit val encoder: Encoder[CustomVideoMessage] = Encoder.instance[CustomVideoMessage] {
    case m@CustomVideoMessage(to, video, customServiceInfo) => Json.obj(
      "touser" -> to.asJson,
      "msgtype" -> m.msgType.asJson,
      "video" -> video.asJson,
      "customservice" -> customServiceInfo.asJson
    )
  } mapJson (_.dropNullValues)

  implicit val decoder: Decoder[CustomVideoMessage] = Decoder.instance { c =>
    (c.get[String]("touser"),
      c.get[CustomVideoMessageContent]("video"),
      c.get[Option[CustomSupportAccount]]("customservice")).mapN(CustomVideoMessage.apply)
  }
}


case class CustomVideoMessageContent(mediaId: String, thumbMediaId: String, title: String, description: String)

object CustomVideoMessageContent {

  import io.circe.derivation._

  implicit val encoder: Encoder.AsObject[CustomVideoMessageContent] = deriveEncoder(derivation.renaming.snakeCase)
  implicit val decoder: Decoder[CustomVideoMessageContent] = deriveDecoder(derivation.renaming.snakeCase)
}

case class CustomMusicMessage(to: String, music: CustomMusicMessageContent, customServiceInfo: Option[CustomSupportAccount] = None) extends CustomMessage {
  override def msgType: String = "music"
}

object CustomMusicMessage {
  implicit val encoder: Encoder[CustomMusicMessage] = Encoder.instance[CustomMusicMessage] {
    case m@CustomMusicMessage(to, music, customServiceInfo) => Json.obj(
      "touser" -> to.asJson,
      "type" -> m.msgType.asJson,
      "music" -> music.asJson,
      "customservice" -> customServiceInfo.asJson
    )
  } mapJson (_.dropNullValues)

  implicit val decoder: Decoder[CustomMusicMessage] = Decoder.instance { c =>
    (c.get[String]("touser"),
      c.get[CustomMusicMessageContent]("music"),
      c.get[Option[CustomSupportAccount]]("customservice")).mapN(CustomMusicMessage.apply)
  }
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

object CustomInternetNewsMessage {
  implicit val encoder: Encoder[CustomInternetNewsMessage] = Encoder.instance[CustomInternetNewsMessage] {
    case m@CustomInternetNewsMessage(to, news, customServiceInfo) => Json.obj(
      "touser" -> to.asJson,
      "type" -> m.msgType.asJson,
      "news" -> news.asJson,
      "customservice" -> customServiceInfo.asJson
    )
  } mapJson (_.dropNullValues)

  implicit val decoder: Decoder[CustomInternetNewsMessage] = Decoder.instance[CustomInternetNewsMessage] { c =>
    (c.get[String]("touser"),
      c.get[CustomNewsArticle]("news"),
      c.get[Option[CustomSupportAccount]]("customservice")).mapN(CustomInternetNewsMessage.apply)
  }
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

object CustomMPNewsMessage {
  implicit val encoder: Encoder[CustomMPNewsMessage] = Encoder.instance[CustomMPNewsMessage] {
    case m@CustomMPNewsMessage(to, mpNews, customServiceInfo) => Json.obj(
      "touser" -> to.asJson,
      "type" -> m.msgType.asJson,
      "mpnews" -> mpNews.asJson,
      "customservice" -> customServiceInfo.asJson
    )
  }.mapJson(_.dropNullValues)

  implicit val decoder: Decoder[CustomMPNewsMessage] = Decoder.instance[CustomMPNewsMessage] { c =>
    (c.get[String]("touser"),
      c.get[CustomMediaMessageContent]("mpnews"),
      c.get[Option[CustomSupportAccount]]("customservice")).mapN(CustomMPNewsMessage.apply)
  }
}

case class CustomMenuMessage(to: String, msgMenu: CustomMenuContent, customServiceInfo: Option[CustomSupportAccount] = None) extends CustomMessage {
  override def msgType: String = "msgmenu"
}

object CustomMenuMessage {
  implicit val encoder: Encoder[CustomMenuMessage] = Encoder.instance[CustomMenuMessage] {
    case m@CustomMenuMessage(to, msgMenu, customServiceInfo) => Json.obj(
      "touser" -> to.asJson,
      "type" -> m.msgType.asJson,
      "msgmenu" -> msgMenu.asJson,
      "customservice" -> customServiceInfo.asJson
    )
  }.mapJson(_.dropNullValues)

  implicit val decoder: Decoder[CustomMenuMessage] = Decoder.instance[CustomMenuMessage] { c =>
    (c.get[String]("touser"),
      c.get[CustomMenuContent]("msgmenu"),
      c.get[Option[CustomSupportAccount]]("customservice")).mapN(CustomMenuMessage.apply)
  }
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

object CustomCardMessage {
  implicit val encoder: Encoder[CustomCardMessage] = Encoder.instance[CustomCardMessage] {
    case m@CustomCardMessage(to, card, customServiceInfo) => Json.obj(
      "touser" -> to.asJson,
      "msgtype" -> m.msgType.asJson,
      "card" -> card.asJson,
      "customservice" -> customServiceInfo.asJson
    )
  }.mapJson(_.dropNullValues)
  implicit val decoder: Decoder[CustomCardMessage] = Decoder.instance[CustomCardMessage] { c =>
    (c.get[String]("touser"),
      c.get[CustomCardContent]("card"),
      c.get[Option[CustomSupportAccount]]("customservice")).mapN(CustomCardMessage.apply)
  }
}

case class CustomCardContent(cardId: String)

object CustomCardContent {

  import io.circe.derivation._

  implicit val encoder: Encoder.AsObject[CustomCardContent] = deriveEncoder(derivation.renaming.snakeCase)
  implicit val decoder: Decoder[CustomCardContent] = deriveDecoder(derivation.renaming.snakeCase)
}

case class CustomMiniProgramMessage(to: String, mini: CustomMiniProgramContent, customServiceInfo: Option[CustomSupportAccount] = None) extends CustomMessage {
  override def msgType: String = "miniprogrampage"
}

object CustomMiniProgramMessage {
  implicit val encoder: Encoder[CustomMiniProgramMessage] = Encoder.instance[CustomMiniProgramMessage] {
    case m@CustomMiniProgramMessage(to, mini, customServiceInfo) => Json.obj(
      "touser" -> to.asJson,
      "type" -> m.msgType.asJson,
      "miniprogrampage" -> mini.asJson,
      "customservice" -> customServiceInfo.asJson
    )
  }.mapJson(_.dropNullValues)

  implicit val decdoer: Decoder[CustomMiniProgramMessage] = Decoder.instance { c =>
    (c.get[String]("touser"),
      c.get[CustomMiniProgramContent]("miniprogrampage"),
      c.get[Option[CustomSupportAccount]]("customservice")).mapN(CustomMiniProgramMessage.apply)
  }
}

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

case class CustomTyping(to: String)

object CustomTyping {
  implicit val encoder: Encoder[CustomTyping] = Encoder.forProduct2("touser", "touser")(ct => (ct.to, "Typing"))
  implicit val decoder: Decoder[CustomTyping] = Decoder.forProduct1("touser")(CustomTyping.apply)
}