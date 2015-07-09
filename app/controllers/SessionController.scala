package controllers

import java.util.UUID

import play.api.mvc.{Security, Action, Controller}
import services.SessionHandlingService

import scala.concurrent.Future


object SessionController {
  val sessionId = "session-id"
}

class SessionController(sessionRepository: SessionHandlingService) extends Controller {

  import scala.concurrent.ExecutionContext.Implicits.global

  def login = Action.async(parse.json) { implicit request =>

    sessionRepository.newSession("user", "topsecret").map { session =>
      Ok.withSession(SessionController.sessionId -> session.id.toString, Security.username -> session.user)
    }
  }

  def logout = Action.async { implicit request =>
    request.session.get(SessionController.sessionId) match {
      case Some(id) =>
        sessionRepository.deleteSession(UUID.fromString(id)).map { result =>
          if (result) Ok.withNewSession else Unauthorized
        }
      case None =>
        Future.successful(Unauthorized)
    }

  }
}

