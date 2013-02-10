package lib

import java.util.Date
import java.text.SimpleDateFormat

import scala.concurrent.Future
import play.api.libs.ws.WS
import play.api.libs.concurrent._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.Logger
import play.api.Play.current
import play.core.parsers._
import play.api.libs.json._
import com.top10.redis._

import akka.actor._
import akka.pattern.pipe

sealed trait GroupActorMsg
case class FetchFeed(graph: facebook.Graph, params: Map[String, String] = Map.empty) extends GroupActorMsg
case class StoreFeed(graph: facebook.Graph, feed: facebook.Feed) extends GroupActorMsg
case object FeedError extends GroupActorMsg

class GroupActor(group: Group) extends Actor {
  def receive = {
    case FetchFeed(graph, params) =>
      Logger.debug("-> fetch feed " + params.toString)
      graph.feed(group.id, params).map { o =>
        // Logger.debug(o.toString)
        o.map { f => StoreFeed(graph, f) } getOrElse FeedError
      } pipeTo self

    case StoreFeed(graph, fbfeed) =>
      Logger.debug("-> store feed")
      val feed = fbfeed.entries.map { fbe =>
        MessageOps.fromFeedEntry(fbe)
      }

      if(feed.isEmpty){
        context.stop(self)
      } else {
        Store.saveFeed(group, feed)

        val params = (FormUrlEncodedParser.parse(fbfeed.nextLink.split("\\?")(1)) - "access_token").mapValues(_.head)

        self ! FetchFeed(graph, params)
      }


  }

}


object FBG {
  def group(id: String)(implicit graph: facebook.Graph) = {
    // Logger.debug(Store.getGroup(id).toString)
    // Logger.debug(Store.getGroup(id).toString)
    Store.getGroup(id, true) match {
      case Some(group) => Akka.future(Some(group))
      case None =>
        graph.group(id).map { fbgOpt => fbgOpt.map { fbgroup =>
          val group = Group(id, fbgroup.name, fbgroup.description, false)
          Store.saveGroup(group)
          group
        } }
    }
  }

  def tag(id: String, tag: String) = {
    Store.getGroup(id).map { group =>
      (group, Store.tag(group, tag))
    }
  }

  def sync(group: Group)(implicit graph: facebook.Graph) =
    Akka.system.actorOf(Props(new GroupActor(group))) ! FetchFeed(graph)
}

case class Group(
  id: String,
  name: String,
  description: String,
  sync: Boolean,
  feed: List[Message] = Nil
)

case class Message(
  id: String,
  from: String,
  content: String,
  tags: Set[String],
  createdAt: Date // TODO: Use proper datatype here!
)

object MessageOps {
  def fromFeedEntry(fbentry: facebook.FeedEntry) = {
    val date = new SimpleDateFormat("yyy-MM-dd'T'HH:mm:ssZ").parse(fbentry.createdTime)
    Message(fbentry.id, fbentry.from, fbentry.message, extractTags(fbentry.message), date)
  }

  def extractTags(str: String) = "#(\\S+)".r.findAllMatchIn(str).map(_.group(1)).toSet
}

object Store {
  implicit val MessageFormat = Json.format[Message]

  def getGroup(id: String, loadFeed: Boolean = false) = {
    val data = redis.hgetAll(key("group", id))
    for {
      name <- data.get("name")
      desc <- data.get("description")
      sync =  data.get("sync").map(_.toLowerCase == "true") getOrElse false // Note: Safer than str.toBoolean
      feed =  if(loadFeed) getFeed(id) else Nil
    } yield {
      Group(id, name, desc, sync, feed)
    }
  }

  def getFeed(groupId: String): List[Message] = {
    val ids = redis.zrevrange(key("group", groupId, "feed"), 0, 100).toList
    fetchFeed(groupId, ids)
  }



  def saveGroup(group: Group) =
    redis.hmset(key("group", group.id), Map(
      "name"        -> group.name,
      "description" -> group.description
    ))

  def saveFeed(group: Group, feed: List[Message]) =
    for(msg <- feed){
      redis.exec(pipe => {
        val js = Json.stringify(Json.toJson(msg))
        pipe.set(key("group", group.id, "feeds", msg.id), js)
        pipe.zadd(key("group", group.id, "feed"), msg.createdAt.getTime, msg.id)
        for(tag <- msg.tags){
          pipe.sadd(key("group", group.id, "tags", tag), msg.id)
        }
      })
    }

  def tag(group: Group, tag: String) = {
    val ids = redis.smembers(key("group", group.id, "tags", tag)).toList
    fetchFeed(group.id, ids)
  }

  def fetchFeed(groupId: String, ids: List[String]) =
    ids.flatMap { id => // XXX: There is no MGET in scala-redis-client
      redis.get(key("group", groupId, "feeds", id)).flatMap { str =>
        Json.fromJson[Message](Json.parse(str)).asOpt
      }.toList
    }.toList

  lazy val redis = new SingleRedis("localhost", 6379)

  lazy val namespace = "fbg"

  def key(ks: String*) = namespace + ":" + ks.mkString(":")

}
