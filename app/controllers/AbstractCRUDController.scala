package controllers

import models.{UriGenerator, UniqueEntity}
import org.w3.banana.RDF
import org.w3.banana.binder.{ToPG, FromPG}
import play.api.libs.json.{JsError, Json, Reads, Writes}
import play.api.mvc.{Action, Controller}
import utils.Global
import scala.util.{Failure, Success}

abstract class AbstractCRUDController[T <: UniqueEntity, R <: RDF, G <: UriGenerator[T]] extends Controller {

  val repo = Global.repo

  implicit def reads: Reads[T]
  implicit def writes: Writes[T]

  implicit def rdfWrites: ToPG[R, T]
  implicit def rdfReads: FromPG[R, T]

  // POST /Ts
  def create() = Action(parse.json) { implicit request =>
    request.body.validate[T].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toFlatJson(errors)
        ))
      },
      success => {
        repo.add[T](success) match {
          case Success(graph) =>
            Created(Json.obj(
              "status" -> "OK",
              "id" -> graph.toString //TODO: determine resource id
            ))
          case Failure(e) =>
            InternalServerError(Json.obj(
              "status" -> "KO",
              "errors" -> Seq(e.toString)
            ))
        }
      }
    )
  }

  // GET /Ts/:id
  def get(id: String) = Action { implicit request =>
    repo.get[T](id) match {
      case Success(s) =>
        Json.toJson(s).asOpt match {
          case Some(t) =>
            Ok(t)
          case None =>
            NotFound(Json.obj(
              "status" -> "KO",
              "message" -> "No such element..."
            ))
        }
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> Seq(e.toString)
        ))
    }
  }

  // GET /ts
  def all() = Action { implicit request =>
    repo.get[T] match {
      case Success(s) =>
        Json.toJson(s).asOpt match {
          case Some(t) =>
            Ok(t)
          case None =>
            NotFound(Json.obj(
              "status" -> "KO",
              "message" -> "No such element..."
            ))
        }
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> Seq(e.toString)
        ))
    }
  }

  def update(id: String) = Action { implicit request =>
    repo.get[T](id) match {
      case Success(s) =>
        s match {
          case Some(t) =>
            repo.update[T, G](t) match {
              case Success(m) =>
                Ok(Json.obj(
                  "status" -> "OK",
                  "id" -> m.toString //TODO: determine subject/resource id
                ))
              case Failure(e) =>
                InternalServerError(Json.obj(
                  "status" -> "KO",
                  "errors" -> Seq(e.toString)
                ))
            }
          case None =>
            NotFound(Json.obj(
              "status" -> "KO",
              "message" -> "No such element..."
            ))
        }
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> Seq(e.toString)
        ))
    }
  }

  def delete(id: String) = ???
}
