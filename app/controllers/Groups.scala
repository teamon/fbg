package controllers

import lib._
import play.api._
import play.api.mvc._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.concurrent.Akka


object Groups extends Controller {

  def index = GraphAction { graph =>
    Async { graph.groups.map(gs => Ok(views.html.groups.index(gs))) }
  }

  def show(id: String) = GraphAction { implicit graph =>
    Async { FBG.group(id).map { opt => opt match {
      case Some(group)  => Ok(views.html.groups.show(group))
      case None         => NotFound
    } } }
  }

  def sync(id: String) = GraphAction { implicit graph =>
    Async { FBG.group(id).map { opt => opt match {
      case Some(group)  =>
        FBG.sync(group)
        Redirect(routes.Groups.show(group.id))
      case None =>
        NotFound
    } } }
  }

  def tags(id: String) = GraphAction { implicit graph =>
    FBG.tags(id) match {
      case Some((group, tags))  => Ok(views.html.groups.tags(group, tags.sorted))
      case None                 => NotFound
    }
  }

  def tag(id: String, tag: String) = GraphAction { implicit graph =>
    FBG.tag(id, tag) match {
      case Some((group, messages))  => Ok(views.html.groups.tag(group, tag, messages))
      case None                     => NotFound
    }
  }

  def GraphAction[A](f: facebook.Graph => Result) = Action { request =>
    request.session.get("token") match {
      case Some(token)  => f(facebook.Graph(token))
      case None         => Redirect(routes.Application.index)
    }
  }
}
