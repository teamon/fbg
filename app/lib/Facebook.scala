package lib.facebook

import scala.concurrent.Future
import play.api.libs.ws.WS
import play.api.libs.concurrent._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.Logger
import play.api.Play.current
import play.core.parsers._
import play.api.libs.json._


case class Auth(
  appId: String,
  appSecret: String
) extends lib.OAuth2(
  appId,
  appSecret,
  "https://www.facebook.com/dialog/oauth/",
  "https://graph.facebook.com/oauth/access_token"
){
  def authenticate(redirect: String, code: String) = requestAccessToken(redirect, code)

  def signIn(scope: List[String], redirect: String) = signInUrl(redirect, mkparams(scope))

  def mkparams(scope: List[String]) = Map("scope" -> scope.mkString(","))
}

case class Graph(token: String){
  import Formats._

  def groups: Future[List[Group]] = get("/me/groups").map { js =>
    ((js \ "data").validate[List[Group]] getOrElse Nil).sortBy(_.order)
  }

  def group(id: String) = obj[GroupDetails](id)

  def feed(id: String, params: Map[String, String]) = obj[Feed](lib.URL(id + "/feed", params))

  def obj[T:Reads](id: String): Future[Option[T]] = get("/" + id).map { js =>
    val res = js.validate[T]
    // Logger.debug(res.toString)
    res.asOpt
  }

  def get(path: String) =
    WS.url(lib.URL("https://graph.facebook.com" + path, Map("access_token" -> token))).get.map { response =>
      // Logger.debug("=== response ===")
      // Logger.debug(response.body)
      Json.parse(response.body)
    }
}


case class Group(
  id: String,
  name: String,
  order: Int
)

case class GroupDetails(
  id: String,
  name: String,
  description: String
)

case class Feed(
  entries: List[FeedEntry],
  prevLink: String,
  nextLink: String
)

case class FeedEntry(
  id: String,
  from: String,
  typ: String,
  message: Option[String],
  picture: Option[String],
  link: Option[String],
  name: Option[String],
  caption: Option[String],
  description: Option[String],
  icon: Option[String],
  createdTime: String
)

object Formats {
  implicit val GroupFormat: Reads[Group] = new Reads[Group] {
    def reads(json: JsValue): JsResult[Group] = for {
      id    <- (json \ "id").validate[String]
      name  <- (json \ "name").validate[String]
      order <- (json \ "bookmark_order").validate[Int]
    } yield Group(id, name, order)
  }

  implicit val GroupDetailsFormat: Reads[GroupDetails] = new Reads[GroupDetails] {
    def reads(json: JsValue): JsResult[GroupDetails] = for {
      id            <- (json \ "id").validate[String]
      name          <- (json \ "name").validate[String]
      description   <- (json \ "description").validate[String]
    } yield GroupDetails(id, name, description)
  }

  implicit val FeedEntryFormat: Reads[FeedEntry] = new Reads[FeedEntry] {
    def reads(json: JsValue): JsResult[FeedEntry] = for {
      id          <- (json \ "id").validate[String]
      from        <- (json \ "from" \ "name").validate[String]
      typ         <- (json \ "type").validate[String]
      message     <- (json \ "message").validate[Option[String]]
      picture     <- (json \ "picture").validate[Option[String]]
      link        <- (json \ "link").validate[Option[String]]
      name        <- (json \ "name").validate[Option[String]]
      caption     <- (json \ "caption").validate[Option[String]]
      description <- (json \ "description").validate[Option[String]]
      icon        <- (json \ "icon").validate[Option[String]]
      createdTime <- (json \ "created_time").validate[String]
    } yield FeedEntry(
      id,
      from,
      typ,
      message,
      picture,
      link,
      name,
      caption,
      description,
      icon,
      createdTime
    )
  }

  implicit val FeedFormat: Reads[Feed] = new Reads[Feed] {
    def reads(json: JsValue): JsResult[Feed] = for {
      entries <- (json \ "data").validate[List[FeedEntry]]
      prev    <- (json \ "paging" \ "previous").validate[String]
      next    <- (json \ "paging" \ "next").validate[String]
    } yield Feed(entries, prev, next)

  }
}
