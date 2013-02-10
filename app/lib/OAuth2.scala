package lib

import scala.concurrent.Future
import play.api.libs.ws.WS
import play.api.libs.concurrent._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.Logger
import play.core.parsers._

object URL {
  def apply(base: String, params: Map[String, String] = Map.empty) = {
    val query = params.map { case (k,v) => k + "=" + java.net.URLEncoder.encode(v, "UTF-8") }.mkString("&")
    val sep = if(base.contains('?')) "&" else "?"
    base + sep + query
  }
}

class OAuth2(
  clientId: String,
  clientSecret: String,
  oauthSignInUrl: String,
  oauthAccessTokenUrl: String
){
  def signInUrl(redirect: String, params: Map[String, String]) = URL(oauthSignInUrl, Map(
    "client_id"     -> clientId,
    "redirect_uri"  -> redirect
  ) ++ params)

  protected def requestAccessToken(redirect: String, code: String): Future[Option[String]] =
    WS.url(requestAccessTokenUrl(redirect, code)).get.map { response =>
      Logger.debug(response.body)
      FormUrlEncodedParser.parse(response.body).get("access_token").flatMap(_.headOption)
    }

  protected def requestAccessTokenUrl(redirect: String, code: String, params: Map[String, String] = Map.empty) =
    URL(oauthAccessTokenUrl, Map(
      "client_id"     -> clientId,
      "client_secret" -> clientSecret,
      "redirect_uri"  -> redirect,
      "code"          -> code
    ) ++ params)

}
