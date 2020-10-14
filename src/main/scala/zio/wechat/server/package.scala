package zio.wechat

import java.security.MessageDigest

import zio.{IO, Task, ZIO, ZLayer}

package object server {


  def wechatRequestValidate(signature: String, timestamp: String, nonce: String, echostr: String): ZIO[WechatAppConfiguration, IllegalArgumentException, String] =
    ZIO.accessM[WechatAppConfiguration](c => {
      val config = c.get
      val sortedString = Seq(config.token,timestamp,nonce).sorted.mkString
      val sha1 = MessageDigest.getInstance("SHA-1").digest(sortedString.getBytes).map("%02X" format _).mkString
      if (signature == sha1){
        IO.succeed(echostr)
      }else{
       IO.fail(new IllegalArgumentException("wrong signature"))
      }
    })

}
