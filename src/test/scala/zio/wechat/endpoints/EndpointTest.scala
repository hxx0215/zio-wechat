package zio.wechat.endpoints

import zio.wechat.model.AccessTokenResponse

object EndpointTest extends App {
  import sttp.client._
  import sttp.tapir.client.sttp._
  implicit val backend: SttpBackend[Identity, Nothing, NothingT] = HttpURLConnectionBackend()

  val accessTokenRequest = accessToken.toSttpRequest(uri"https://api.weixin.qq.com").apply(("client_credential","wx3595fa5d8cafd522","a53837bd6f161226111efe79c4f0445a"))
  val result = accessTokenRequest.send().body



//  val x = result.getOrElse(Right(Right(AccessTokenResponse("0",1))))
  println(result)
}
