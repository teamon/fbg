package controllers

import lib._
import play.api._
import play.api.mvc._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.concurrent.Akka


object Auth extends Controller {

  val oauth = facebook.Auth(
    Play.configuration.getString("facebook.appId") getOrElse "",
    Play.configuration.getString("facebook.appSecret") getOrElse ""
  )

  def signIn = Action { implicit request =>
    Redirect(oauth.signIn(
      List("user_groups"),
      routes.Auth.callback.absoluteURL()
    ))
  }

  def callback = Action { implicit request =>
    request.getQueryString("code").map { code =>
      Async {
        oauth.authenticate(routes.Auth.callback.absoluteURL(), code).map { resp => resp match {
          case Some(token)  => Redirect(routes.Application.index).withSession("token" -> token)
          case None         => Redirect(routes.Auth.signIn)
        } }
      }
    } getOrElse {
      Redirect(routes.Auth.signIn)
    }
  }
}
