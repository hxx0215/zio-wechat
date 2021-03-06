package zio.wechat.model

import enumeratum.{Enum, EnumEntry}
import io.circe.generic.extras.{ConfiguredJsonCodec, JsonKey}

sealed abstract class QRCodeActionName(override val entryName: String) extends EnumEntry

sealed abstract class QRCodeLimitActionName(override val entryName: String) extends EnumEntry

object QRCodeActionName extends Enum[QRCodeActionName] {
  val values = findValues

  case object QRSCENE extends QRCodeActionName("QR_SCENE")

  case object QRSTRSCENE extends QRCodeActionName("QR_STR_SCENE")


}

object QRCodeLimitActionName extends Enum[QRCodeLimitActionName] {
  val values = findValues

  case object QRLIMITSCENE extends QRCodeActionName("QR_LIMIT_SCENE")

  case object QRLIMITSTRSCENE extends QRCodeActionName("QR_LIMIT_STR_SCENE")

}

sealed trait QRCodeRequest

case class TemporaryQRCodeRequest(expireSeconds: Int, actionName: QRCodeActionName, actionInfo: QRCodeActionInfo) extends QRCodeRequest

case class PermanentQRCodeRequest(actionName: QRCodeLimitActionName, actionInfo: QRCodeActionInfo) extends QRCodeRequest


case class QRCodeActionInfo(scene: QRCodeScene)

sealed trait QRCodeScene

@ConfiguredJsonCodec case class QRCodeIdScene(@JsonKey("scene_id") scene: Int) extends QRCodeScene

@ConfiguredJsonCodec case class QRCodeStringScene(@JsonKey("scene_str")scene: String) extends QRCodeScene


case class QRCodeResponse(ticket: String, expireSeconds: Int, url: String)

