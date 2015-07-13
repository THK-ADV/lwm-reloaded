package controllers

import java.util.UUID

import models.{ValidationFailure, Session, Login}
import play.api.libs.json.{JsError, Json}
import play.api.mvc.{Security, Action, Controller}
import services.SessionHandlingService

import scala.concurrent.Future

object SessionController {
  val sessionId = "session-id"
}

class SessionController(sessionRepository: SessionHandlingService) extends Controller {

  import scala.concurrent.ExecutionContext.Implicits.global

  def login = Action.async(parse.json) { implicit request =>
    request.body.validate[Login].fold (
      errors => {
        Future.successful(BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        )))
      },
      success => {
        sessionRepository.newSession(success.username, success.password).map {
          case s: Session =>
            Ok.withSession(SessionController.sessionId -> s.id.toString, Security.username -> s.user)

          case f: ValidationFailure => Unauthorized(Json.obj(
            "status" -> "KO",
            "errors" -> f.s
          ))
        }
      }
    )
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

