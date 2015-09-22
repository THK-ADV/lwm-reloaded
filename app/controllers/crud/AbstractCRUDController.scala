package controllers.crud

import java.util.UUID

import models.{UniqueEntity, UriGenerator}
import modules.BaseNamespace
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc._
import services.RoleService
import store.SesameRepository
import store.bind.Bindings
import utils.LWMActions.ContentTypedAction
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Failure, Success}

trait SesameRdfSerialisation[T <: UniqueEntity] { self: BaseNamespace =>

  def repository: SesameRepository

  def defaultBindings: Bindings[Sesame] = Bindings[Sesame](namespace)

  implicit def rdfWrites: ToPG[Sesame, T]

  implicit def rdfReads: FromPG[Sesame, T]

  implicit def classUrisFor: ClassUrisFor[Sesame, T]

  implicit def uriGenerator: UriGenerator[T]
}

trait JsonSerialisation[I, O] {
  implicit def reads: Reads[I]

  implicit def writes: Writes[O]
}

trait Filterable {
  def getWithFilter(queryString: Map[String, Seq[String]]): Result
}

trait ModelConverter[I, O] {
  protected def fromInput(input: I, id: Option[UUID] = None): O
}

trait ContentTyped {
  implicit val mimeType: LwmMimeType
}

trait Secured {
  implicit val roleService: RoleService
}

trait AbstractCRUDController[I, O <: UniqueEntity] extends Controller
with JsonSerialisation[I, O]
with SesameRdfSerialisation[O]
with Filterable
with ModelConverter[I, O]
with BaseNamespace
with ContentTyped
with Secured {

  // POST /Ts
  def create = ContentTypedAction { implicit request =>
    request.body.validate[I].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => {
        repository.add[O](fromInput(success)) match {
          case Success(graph) =>
            Created(Json.toJson(rdfReads.fromPG(graph).get)).as(mimeType)
          case Failure(e) =>
            InternalServerError(Json.obj(
              "status" -> "KO",
              "errors" -> e.getMessage
            ))
        }
      }
    )
  }

  // GET /Ts/:id
  def get(id: String) = Action { implicit request =>
    val uri = s"$namespace${request.uri}"

    repository.get[O](uri) match {
      case Success(s) =>
        s match {
          case Some(entity) =>
            Ok(Json.toJson(entity)).as(mimeType)
          case None =>
            NotFound(Json.obj(
              "status" -> "KO",
              "message" -> "No such element..."
            ))
        }
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  // GET /ts with optional queries
  def all() = Action { implicit request =>
    if (request.queryString.isEmpty) {
      repository.get[O] match {
        case Success(s) =>
          Ok(Json.toJson(s)).as(mimeType)
        case Failure(e) =>
          InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> e.getMessage
          ))
      }
    } else {
      getWithFilter(request.queryString)
    }
  }

  def update(id: String) = ContentTypedAction { implicit request =>
    repository.get[O](id) match {
      case Success(s) =>
        s match {
          case Some(t) =>
            request.body.validate[I].fold(
              errors => {
                BadRequest(Json.obj(
                  "status" -> "KO",
                  "errors" -> JsError.toJson(errors)
                ))
              },
              success => {
                repository.update[O, UriGenerator[O]](fromInput(success, Some(t.id))) match {
                  case Success(graph) =>
                    Ok(Json.toJson(rdfReads.fromPG(graph).get)).as(mimeType)
                  case Failure(e) =>
                    InternalServerError(Json.obj(
                      "status" -> "KO",
                      "errors" -> e.getMessage
                    ))
                }
              }
            )
          case None =>
            NotFound(Json.obj(
              "status" -> "KO",
              "message" -> "No such element..."
            ))
        }
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  def delete(id: String) = Action { implicit request =>
    repository.delete(id) match {
      case Success(s) =>
        Ok(Json.obj(
          "status" -> "OK",
          "id" -> s.subjects().iterator().next().toString
        ))
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  def header = Action { implicit request =>
    NoContent.as(mimeType)
  }

}