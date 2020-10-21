package zio.wechat.endpoints

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import io.circe.syntax.EncoderOps
import sttp.tapir.DecodeResult
import zio.wechat.model.{ConcreteMenu, MainMenu, MenuType, QRCodeActionInfo, QRCodeActionName, QRCodeIdScene, TemporaryQRCodeRequest}

object EndpointTest extends App {

  import sttp.client._
  import sttp.tapir.client.sttp._

  implicit val backend: SttpBackend[Identity, Nothing, NothingT] = HttpURLConnectionBackend()

  //  val accessTokenRequest = accessToken.toSttpRequest(uri"https://api.weixin.qq.com").apply(("client_credential", "wx3595fa5d8cafd522", "a53837bd6f161226111efe79c4f0445a"))
  val accessTokenRequest = accessToken.toSttpRequest(uri"https://api.weixin.qq.com").apply(("client_credential", "wxaf99fd18d929117e", "bd46a71de4f309bc20fc98fb144b7e30"))
  val result = accessTokenRequest.send().body

  println(result)

  def qrCodeTest = {
    val qrcodeRequest = generateQRCodeEndpoint[TemporaryQRCodeRequest].toSttpRequest(uri"https://api.weixin.qq.com")
    val showQRCodeRequest = showQRCodeEndpoint.toSttpRequestUnsafe(uri"https://mp.weixin.qq.com")
    val DecodeResult.Value(v) = result
    (for {
      response <- v
      token <- response
      qrcodeResponse <- {
        val DecodeResult.Value(body) =
          qrcodeRequest.apply(token.accessToken, TemporaryQRCodeRequest(1000, QRCodeActionName.QRSCENE, QRCodeActionInfo(QRCodeIdScene(1))))
            .send().body
        body
      }
      qrResponse <- qrcodeResponse
      finalResult <- {
        val encoded = URLEncoder.encode(qrResponse.ticket, StandardCharsets.UTF_8)
        println(encoded)
        val applyResult = showQRCodeRequest.apply(encoded)
        println(applyResult.uri)
        applyResult.send().body
      }
    } yield finalResult).foreach(println)
  }

  def menuTest = {
    val concreteMenu = ConcreteMenu("测试菜单", MenuType.Click, Some("test-menu-key"))
    val mainMenu = MainMenu(Seq(concreteMenu))
    val menuRequest = menuEndpoint.toSttpRequestUnsafe(uri"https://api.weixin.qq.com")
    val DecodeResult.Value(v) = result
    (for {
      response <- v
      token <- response
      body <- menuRequest.apply(token.accessToken, mainMenu).send().body
    } yield {
      body
    }).foreach(println)
  }

  menuTest
}
