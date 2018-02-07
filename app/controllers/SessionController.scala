package controllers

import java.util.UUID

import controllers.helper.{ContentTyped, SessionChecking}
import models.{InvalidSession, Login, ValidSession}
import play.api.libs.json.{JsError, Json}
import play.api.mvc._
import services.SessionHandlingService
import utils.LwmActions.ContentTypedAction
import utils.LwmMimeType

import scala.concurrent.Future
import scala.util.control.NonFatal

object SessionController {
  lazy val sessionId = "session-id"
  lazy val userId = "user-id"
}

class SessionController(val sessionService: SessionHandlingService) extends Controller with SessionChecking with ContentTyped {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit override val mimeType: LwmMimeType = LwmMimeType.loginV1Json

  def login = ContentTypedAction.async { implicit request =>
    request.body.validate[Login].fold(
      errors => {
        Future.successful(BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        )))
      },
      success => {
        sessionService.newSession(success.username.toLowerCase, success.password).map {
          case ValidSession(username, userId, id, _) =>
            Ok.withSession(
              SessionController.sessionId -> id.toString,
              SessionController.userId -> userId.toString,
              Security.username -> username
            ).as(mimeType)
          case InvalidSession(message) =>
            Unauthorized(Json.obj(
              "status" -> "KO",
              "message" -> message
            ))
        }.recover {
          case NonFatal(e) =>
            InternalServerError(Json.obj(
              "status" -> "KO",
              "errors" -> e.getMessage
            ))
        }
      }
    )
  }

  def logout = Action.async { implicit request =>
    request.session.get(SessionController.sessionId) match {
      case Some(id) =>
        sessionService.deleteSession(UUID.fromString(id)).map { result =>
          if (result) Ok.withNewSession else BadRequest
        }
      case None =>
        Future.successful(Unauthorized)
    }
  }

  def header = Action { implicit request =>
    NoContent.as(mimeType)
  }

  def valid = Action.async { implicit request =>
    request.session.get(SessionController.sessionId) match {
      case Some(id) =>
        sessionService.isValid(UUID.fromString(id)).map( result =>
          if (result) Ok else Unauthorized
        )
      case None =>
        Future.successful(Unauthorized)
    }
  }
}