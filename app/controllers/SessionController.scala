package controllers

import java.util.UUID

import controllers.crud.{ContentTyped, SessionChecking}
import models.{Login, Session}
import play.api.libs.json.{JsError, Json}
import play.api.mvc._
import services.SessionHandlingService
import utils.LWMActions.ContentTypedAction
import utils.LwmMimeType

import scala.concurrent.Future
import scala.util.control.NonFatal

object SessionController {
  val sessionId = "session-id"
  val userId = "user-id"
}

class SessionController(val sessionService: SessionHandlingService) extends Controller with SessionChecking with ContentTyped {

  implicit override val mimeType: LwmMimeType = LwmMimeType.loginV1Json

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
        sessionService.newSession(success.username, success.password).map {
          case s: Session =>
            Ok.withSession(
              SessionController.sessionId -> s.id.toString,
              SessionController.userId -> s.userId.toString,
              Security.username -> s.username
            ).as(mimeType)
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