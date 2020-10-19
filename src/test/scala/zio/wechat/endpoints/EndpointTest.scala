package zio.wechat.endpoints

import sttp.tapir.DecodeResult
import zio.wechat.model.{AccessTokenResponse, QRCodeActionInfo, QRCodeActionName, QRCodeIdScene, TemporaryQRCodeRequest}

object EndpointTest extends App {

  import sttp.client._
  import sttp.tapir.client.sttp._

  implicit val backend: SttpBackend[Identity, Nothing, NothingT] = HttpURLConnectionBackend()

  val accessTokenRequest = accessToken.toSttpRequest(uri"https://api.weixin.qq.com").apply(("client_credential", "wx3595fa5d8cafd522", "a53837bd6f161226111efe79c4f0445a"))
  val result = accessTokenRequest.send().body

  println(result)

  val qrcodeRequest = temporaryQRCode.toSttpRequest(uri"https://api.weixin.qq.com")
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
  } yield qrcodeResponse).foreach(println)
}
