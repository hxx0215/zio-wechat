package zio.wechat

import java.security.MessageDigest

import zio.{IO, Task, ZIO, ZLayer}

package object server {


  def wechatRequestValidate(signature: String, timestamp: String, nonce: String, echostr: String): Task[String] = {
    wechatConfig.memoize.flatten >>= (config => {
      val sortedString = Seq(config.token, timestamp, nonce).sorted.mkString
      val sha1 = MessageDigest.getInstance("SHA-1").digest(sortedString.getBytes).map("%02x" format _).mkString
      if (signature == sha1) {
        IO.succeed(echostr)
      } else {
        IO.fail(new IllegalArgumentException("wrong signature"))
      }
    })

  }

}
