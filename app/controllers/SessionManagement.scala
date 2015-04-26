package controllers

import akka.util.Timeout
import utils.SessionHandler
import play.api.mvc.{Security, Action, Controller}
import play.libs.Akka
import play.api.libs.concurrent.{Promise ⇒ PlayPromise}
import scala.concurrent.Future
import scala.util.control.NonFatal

object SessionManagement extends Controller {

  import akka.pattern.ask

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  private implicit val timeout = Timeout(60.seconds)
  private val sessionsHandler = Akka.system.actorSelection("user/sessions")

  def login() = Action.async(parse.json) { implicit request ⇒
    val user = (request.body \ "username").asOpt[String]
    val pass = (request.body \ "password").asOpt[String]

    if (user.isDefined && pass.isDefined) {
      val timeoutFuture = PlayPromise.timeout("No response from IDM", 45.second)
      val authFuture = (sessionsHandler ? SessionHandler.AuthenticationRequest(user.get.toLowerCase, pass.get)).mapTo[Either[String, SessionHandler.Session]]

      Future.firstCompletedOf(Seq(authFuture, timeoutFuture)).map {
        case Left(message: String) ⇒ Unauthorized(message)

        case Right(session: SessionHandler.Session) ⇒
          Ok("200: Session created").withSession(
            Security.username -> user.get,
            "session" -> session.id
          )
      }.recover {
        case NonFatal(e) ⇒
          InternalServerError(s"Oops. There seems to be a problem ($e) with the server. We are working on it!")
      }
    }
    else {
      Future(Unauthorized)
    }
  }

  def logout() = Action { request ⇒
    request.session.get("session").map(sessionsHandler ! SessionHandler.LogoutRequest(_))
    Ok("Session removed")
  }
}