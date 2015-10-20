package controllers.crud

import java.util.UUID

import models.security.Permission
import models.{UniqueEntity, UriGenerator}
import modules.BaseNamespace
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc._
import services.RoleService
import store.SesameRepository
import store.bind.Bindings
import utils.LWMActions._
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Failure, Success}

trait SesameRdfSerialisation[T <: UniqueEntity] {
  self: BaseNamespace =>

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

trait Filterable[O] {
  def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[O]): Result
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

/**
 * `Deferred` provides an algebra for separately and indirectly composing
 * a `SecureAction` with the `Permission`s required to run that `SecureAction`.
 *
 * Each controller has specialised restrictions for their respective
 * CRUD operations. This means that they must somehow be "deferred to"
 * the generalised specification of these restrictions.
 *
 * `Deferred` grants this possibility.
 */
trait Deferred {
  self: Secured with ContentTyped =>

  sealed trait Rule

  case object Create extends Rule

  case object Delete extends Rule

  case object All extends Rule

  case object Get extends Rule

  case object Update extends Rule

  case class Invoke(run: Rule => Block)

  case class Block(restrictions: (Option[String], Set[Permission])) {

    /**
     * Invocation of a `SecureAction`.
     * `Block` feeds its restrictions to the `SecureAction`.
     *
     * @param block Function block
     * @return Action
     */
    def secured(block: Request[AnyContent] => Result): Action[AnyContent] = restrictions match {
      case (o, s) => SecureAction((o.map(UUID.fromString), s))(block)
    }

    /**
     * Invocation of a `SecureContentTypedAction`.
     * `Block` feeds its restrictions to the `SecureContentTypedAction`.
     *
     * @param block Function block
     * @return Action
     */
    def secureContentTyped(block: Request[JsValue] => Result): Action[JsValue] = restrictions match {
      case (o, s) => SecureContentTypedAction((o.map(UUID.fromString), s))(block)
    }
  }

  /**
   * Allows `Action` invocations based on restrictions defined by the `Rule`.
   * Specialisations of this define a set of restrictions for each `Rule`.
   *
   *  i.e: Invoke {
   *    case Create => ..restrictions
   *    case Delete => ..restrictions
   *    ..
   *  }
   *
   * These restrictions are then fed to a `SecureAction` that can be invoked
   * by using the functions defined on `Block`.
   *
   * [`moduleId` is added as an additional dependency, because some controllers
   * depend on module restrictions to function properly. Specialisations can simply omit this
   * parameter if they haven't need of it]
   *
   * @param rule Rule referencing operation restrictions
   * @param moduleId possible module id
   * @return Invoke Block
   */
  protected def invokeAction(rule: Rule)(moduleId: Option[String]): Block = Block((None, Set()))
}

trait AbstractCRUDController[I, O <: UniqueEntity] extends Controller
with JsonSerialisation[I, O]
with SesameRdfSerialisation[O]
with Filterable[O]
with ModelConverter[I, O]
with BaseNamespace
with ContentTyped
with Secured
with Deferred {

  // POST /Ts
  def create = invokeAction(Create)(None) secureContentTyped { implicit request =>
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
  def get(id: String) = invokeAction(Get)(Some(id)) secured { implicit request =>
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
  def all() = invokeAction(All)(None) secured { implicit request =>
    repository.get[O] match {
      case Success(s) =>
        if (request.queryString.isEmpty)
          Ok(Json.toJson(s)).as(mimeType)
        else
          getWithFilter(request.queryString)(s)
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }


  def update(id: String) = invokeAction(Update)(Some(id)) secureContentTyped { implicit request =>
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

  def delete(id: String) = invokeAction(Delete)(Some(id)) secured { implicit request =>
    import collection.JavaConversions._
    repository.delete(id) match {
      case Success(s) =>
        Ok(Json.obj(
          "status" -> "OK",
          "id" -> s.subjects().iterator().toVector.mkString(" ")
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