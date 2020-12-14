package zio.wechat.model

import java.io.File
import scala.collection.immutable.Seq

import sttp.model.Part
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import cats.implicits._

object Assets {

}

case class MediaForm(media: Part[File])

case class UrlResponse(url: String)

object UrlResponse {
  implicit val encoder: Encoder[UrlResponse] = deriveEncoder
  implicit val decoder: Decoder[UrlResponse] = deriveDecoder
}

case class AssetResponse(`type`: String, mediaId: String, createAt: Int)

object AssetResponse {

  import io.circe.derivation._

  implicit val encoder: Encoder[AssetResponse] = deriveEncoder(derivation.renaming.snakeCase)
  implicit val decoder: Decoder[AssetResponse] = deriveDecoder(derivation.renaming.snakeCase)
}

case class ArticleAsset(thumbMediaId: String,
                        title: String,
                        content: String,
                        author: Option[String] = None,
                        contentSourceUrl: Option[String] = None,
                        digest: Option[String] = None,
                        showCoverPic: Option[Boolean] = None,
                        needOpenComment: Option[Boolean] = None,
                        onlyFansCanComment: Option[Boolean] = None)

object ArticleAsset {
  implicit val encoder: Encoder[ArticleAsset] = Encoder.instance[ArticleAsset] {
    case ArticleAsset(thumbMediaId, title, content, author, contentSourceUrl, digest, showCoverPic, needOpenComment, onlyFansCanComment) => Json.obj(
      "thumb_media_id" -> thumbMediaId.asJson,
      "author" -> author.asJson,
      "title" -> title.asJson,
      "content" -> content.asJson,
      "content_source_url" -> contentSourceUrl.asJson,
      "digest" -> digest.asJson,
      "show_cover_pic" -> showCoverPic.map(p => if (p) 1 else 0).asJson,
      "need_open_comment" -> needOpenComment.map(p => if (p) 1 else 0).asJson,
      "only_fans_can_comment" -> onlyFansCanComment.map(p => if (p) 1 else 0).asJson
    )
  }.mapJson(_.dropNullValues)

  implicit val decoder: Decoder[ArticleAsset] = Decoder.instance(c => (
    c.get[String]("thumb_media_id"),
    c.get[String]("title"),
    c.get[String]("content"),
    c.get[Option[String]]("author"),
    c.get[Option[String]]("content_source_url"),
    c.get[Option[String]]("digest"),
    c.get[Option[Int]]("show_cover_pic").map(_.map(_ == 1)),
    c.get[Option[Int]]("need_open_comment").map(_.map(_ == 1)),
    c.get[Option[Int]]("only_fans_can_comment").map(_.map(_ == 1))
    ).mapN(ArticleAsset.apply))
}

case class ArticleListAsset(articles: Seq[ArticleAsset])

object ArticleListAsset {
  implicit val encoder: Encoder[ArticleListAsset] = Encoder.instance[ArticleListAsset] {
    case ArticleListAsset(articles) => Json.obj(
      "articles" -> articles.asJson
    )
  }
  implicit val decoder: Decoder[ArticleListAsset] = Decoder.instance { c =>
    c.get[Seq[ArticleAsset]]("articles").map(ArticleListAsset.apply)
  }
}

case class BroadcastFilter(isToAll: Boolean, tagId: Int)

object BroadcastFilter {

  import io.circe.derivation._

  implicit val encoder: Encoder[BroadcastFilter] = deriveEncoder(derivation.renaming.snakeCase)
  implicit val decoder: Decoder[BroadcastFilter] = deriveDecoder(derivation.renaming.snakeCase)
}

sealed trait BroadcastMessage {
  def filter: Option[BroadcastFilter]

  def msgType: String

  def to: Option[Seq[String]]
}

case class NewsBroadcastMessage(filter: Option[BroadcastFilter], to: Option[Seq[String]], mpNews: CustomMediaMessageContent) extends BroadcastMessage {
  override def msgType: String = "mpnews"
}

object NewsBroadcastMessage {
  implicit val encoder: Encoder[NewsBroadcastMessage] = Encoder.instance[NewsBroadcastMessage] {
    case m@NewsBroadcastMessage(filter, to, mpNews) => Json.obj(
      "filter" -> filter.asJson,
      "touser" -> to.asJson,
      "mpnews" -> mpNews.asJson,
      "msgtype" -> m.msgType.asJson
    )
  }.mapJson(_.dropNullValues)
}