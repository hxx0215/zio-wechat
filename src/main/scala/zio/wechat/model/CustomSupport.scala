package zio.wechat.model

import scala.collection.immutable.Seq
import java.io.File

import io.circe._
import io.circe.syntax._
import sttp.model.Part

case class UpdateCustomSupportMessage(account: String, nickname: String, password: String)

object UpdateCustomSupportMessage {
  implicit val encoder: Encoder[UpdateCustomSupportMessage] = Encoder.forProduct3("kf_account", "nickname", "password")(UpdateCustomSupportMessage.unapply(_).get)
  implicit val decoder: Decoder[UpdateCustomSupportMessage] = Decoder.forProduct3("kf_account", "nickname", "password")(UpdateCustomSupportMessage.apply)
}

case class RemoveCustomSupportMessage(account: String)

object RemoveCustomSupportMessage {
  implicit val encoder: Encoder[RemoveCustomSupportMessage] = Encoder.forProduct1("kf_account")(RemoveCustomSupportMessage.unapply(_).get)
  implicit val decoder: Decoder[RemoveCustomSupportMessage] = Decoder.forProduct1("kf_account")(RemoveCustomSupportMessage.apply)
}

case class CustomSupportAvatarForm(media: Part[File])

case class CustomSupportList(list: Seq[CustomSupportInfo])

object CustomSupportList {
  implicit val encoder: Encoder[CustomSupportList] = Encoder.instance {
    case CustomSupportList(list) => Json.obj(
      "kf_list" -> list.asJson
    )
  }
}


case class CustomSupportInfo(account: String, nick: String, id: String, imageURL: String)

object CustomSupportInfo {
  implicit val encoder: Encoder[CustomSupportInfo] = Encoder.forProduct4("kf_account", "kf_nick", "kf_id", "kf_headimgurl")(CustomSupportInfo.unapply(_).get)
  implicit val decoder: Decoder[CustomSupportInfo] = Decoder.forProduct4("kf_account", "kf_nick", "kf_id", "kf_headimgurl")(CustomSupportInfo.apply)
}

