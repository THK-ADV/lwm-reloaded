package controllers.crud

import java.util.UUID

import models.{UniqueEntity, UriGenerator}
import modules.BaseNamespace
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsError, Json, Reads, Writes}
import play.api.mvc.{Result, Action, Controller}
import store.bind.Bindings
import store.{Namespace, SesameRepository}

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

trait AbstractCRUDController[I, O <: UniqueEntity] extends Controller
with JsonSerialisation[I, O]
with SesameRdfSerialisation[O]
with Filterable
with ModelConverter[I, O]
with BaseNamespace {

  // POST /Ts
  def create() = Action(parse.json) { implicit request =>
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
            Created(Json.obj(
              "status" -> "OK",
              "id" -> graph.subjects().iterator().next().toString
            ))
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
    println(uri)

    repository.get[O](uri) match {
      case Success(s) =>
        s match {
          case Some(entity) =>
            Ok(Json.toJson(entity))
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
          Ok(Json.toJson(s))
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

  def update(id: String) = Action(parse.json) { implicit request =>
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
                  case Success(m) =>
                    Ok(Json.obj(
                      "status" -> "OK",
                      "id" -> m.subjects().iterator().next().toString
                    ))
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
}
