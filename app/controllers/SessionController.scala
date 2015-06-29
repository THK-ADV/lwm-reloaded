package controllers

import java.util.UUID

import play.api.mvc.{Action, Controller}
import services.SessionHandlingService

import scala.concurrent.Future


class SessionController(sessionRepository: SessionHandlingService) extends Controller {

  import scala.concurrent.ExecutionContext.Implicits.global

  def login = Action.async(parse.json) { implicit request =>
    sessionRepository.newSession("").map { session =>
      Ok.withSession("session-id" -> session.id.toString)
    }
  }

  def logout = Action.async { implicit request =>
    request.session.get("session-id") match {
      case Some(id) =>
        sessionRepository.deleteSession(UUID.fromString(id)).map { result =>
          if (result) Ok.withNewSession else Unauthorized
        }
      case None =>
        Future.successful(Unauthorized)
    }

  }
}

