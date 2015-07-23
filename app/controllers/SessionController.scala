package controllers

import java.util.UUID

import models.{Login, Session}
import play.api.libs.json.{JsError, Json}
import play.api.mvc._
import services.SessionHandlingService
import utils.{LWMBodyParser, LWMMimeType}

import scala.concurrent.Future

object SessionController {
  val sessionId = "session-id"
  val mimeType = LWMMimeType.loginV1Json
}

class SessionController(sessionRepository: SessionHandlingService) extends Controller {

  import scala.concurrent.ExecutionContext.Implicits.global

  def login = Action.async(LWMBodyParser.parseWith(SessionController.mimeType)) { implicit request =>
    request.body.validate[Login].fold(
      errors => {
        Future.successful(BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        )))
      },
      success => {
        sessionRepository.newSession(success.username, success.password).map {
          case s: Session =>
            Ok.withSession(
              SessionController.sessionId -> s.id.toString,
              Security.username -> s.user
            ).as(SessionController.mimeType)
        }
      }
    )
  }

  def logout = Action.async { implicit request =>
    request.session.get(SessionController.sessionId) match {
      case Some(id) =>
        sessionRepository.deleteSession(UUID.fromString(id)).map { result =>
          if (result) Ok.withNewSession else BadRequest
        }
      case None =>
        Future.successful(Unauthorized)
    }
  }

  def header = Action { implicit request =>
    NoContent.as(SessionController.mimeType)
  }
}