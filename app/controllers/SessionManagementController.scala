package controllers

import akka.util.Timeout
import play.api.Play.current
import utils.SessionHandler._
import play.api.mvc.{Security, Action, Controller}
import play.libs.Akka
import play.api.libs.concurrent.{Promise ⇒ PlayPromise}
import utils.SessionHandler
import scala.concurrent.Future
import scala.util.control.NonFatal

object SessionManagementController extends Controller {

  import akka.pattern.ask

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  private implicit val timeout = Timeout(60.seconds)
  private val sessionsHandler = Akka.system.actorSelection("user/sessions")
  private val controllerTimeout = current.configuration.getInt("lwm.controllers.timeout").get

  def login() = Action.async(parse.json) { implicit request ⇒
    val user = (request.body \ "username").asOpt[String]
    val pass = (request.body \ "password").asOpt[String]

    if (user.isDefined && pass.isDefined) {
      val authFuture = sessionsHandler ? SessionHandler.AuthenticationRequest(user.get.toLowerCase, pass.get)

      (authFuture map {
        case SessionHandler.AuthenticationFailure(msg: String) ⇒ Unauthorized(msg)

        case SessionHandler.AuthenticationSuccess(session: SessionHandler.Session) ⇒
          Ok("Session created").withSession(
            Security.username -> user.get,
            "session" -> session.id
          )
      }).recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
      }
    }
    else {
      Future(Unauthorized)
    }
  }

  def logout() = Action { request ⇒
    request.session.get("session").foreach(sessionsHandler ! SessionHandler.LogoutRequest(_))
    Ok("Session removed")
  }
}