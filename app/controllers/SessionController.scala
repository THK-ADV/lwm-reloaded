package controllers

import java.util.UUID

import controllers.crud.ContentTyped
import models.{Login, Session}
import play.api.libs.json.{JsError, Json}
import play.api.mvc._
import services.SessionHandlingService
import utils.{ContentTypedAction, LWMBodyParser, LWMMimeType}

import scala.concurrent.Future

object SessionController {
  val sessionId = "session-id"
}

class SessionController(sessionRepository: SessionHandlingService) extends Controller with ContentTyped {

  implicit override val mimeType: LWMMimeType = LWMMimeType.loginV1Json

  import scala.concurrent.ExecutionContext.Implicits.global

  def login = ContentTypedAction.async { implicit request =>
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
            ).as(mimeType)
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
    NoContent.as(mimeType)
  }
}