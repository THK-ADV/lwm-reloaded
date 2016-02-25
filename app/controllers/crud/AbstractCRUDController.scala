package controllers.crud

import java.util.UUID

import models.security.Permission
import models.{UriGenerator, UniqueEntity}
import modules.store.BaseNamespace
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
import scala.concurrent.Future
import scala.util.{Try, Failure, Success}

trait SesameRdfSerialisation[T <: UniqueEntity] {
  self: BaseNamespace =>

  def repository: SesameRepository

  val defaultBindings: Bindings[Sesame] = Bindings[Sesame](namespace)

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
  protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[O]): Try[Set[O]]
}

trait ModelConverter[I, O] {
  protected def fromInput(input: I, id: Option[UUID] = None): O

  protected def atomize(output: O): Try[Option[JsValue]]

  protected def atomizeMany(output: Set[O]): Try[JsValue]
}

trait Consistent[I, O] {
  import store.sparql.select._
  import store.sparql.{NoneClause, Clause, SelectClause}

  protected def compareModel(input: I, output: O): Boolean

  protected def existsQuery(input: I): (Clause, Var) = (NoneClause, v(""))

  def exists(input: I)(repository: SesameRepository): Option[UUID] = {
    val (clause, key) = existsQuery(input)

    clause match {
      case select@SelectClause(_, _) =>
        for {
          map <- repository.query(select)
          list <- map.get(key.v)
          value <- list.headOption
        } yield UUID.fromString(value.stringValue())

      case _ => None
    }
  }
}

trait ContentTyped {
  implicit val mimeType: LwmMimeType
}

trait Secured {
  implicit val roleService: RoleService
}

/**
  * `SecureControllerContext` provides an algebra for separately and indirectly composing
  * a `SecureAction` with the `Permission`s required to run that `SecureAction`.
  *
  * Each controller has specialised restrictions for their respective
  * CRUD operations. This means that they must somehow be "deferred to"
  * the generalised specification of these restrictions.
  *
  */
trait SecureControllerContext {
  self: Secured with ContentTyped =>

  //to be specialized
  protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case _ => NonSecureBlock
  }

  //to be specialized
  protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => NonSecureBlock
  }

  trait SecureContext {

    def apply[A](restricted: (Option[UUID], Set[Permission]) => Action[A], simple: => Action[A]) = this match {
      case SecureBlock(id, set) => restricted(Some(UUID.fromString(id)), set)
      case PartialSecureBlock(set) => restricted(None, set)
      case NonSecureBlock => simple()
    }

    def action(block: Request[AnyContent] => Result): Action[AnyContent] = apply[AnyContent](
      restricted = (opt, perms) => SecureAction((opt, perms))(block),
      simple = Action(block)
    )

    def contentTypedAction(block: Request[JsValue] => Result): Action[JsValue] = apply[JsValue](
      restricted = (opt, perms) => SecureContentTypedAction((opt, perms))(block),
      simple = ContentTypedAction(block)
    )

    def asyncAction(block: Request[AnyContent] => Future[Result]): Action[AnyContent] = apply[AnyContent](
      restricted = (opt, perms) => SecureAction.async((opt, perms))(block),
      simple = Action.async(block)
    )

    def asyncContentTypedAction(block: Request[JsValue] => Future[Result]): Action[JsValue] = apply[JsValue](
      restricted = (opt, perms) => SecureContentTypedAction.async((opt, perms))(block),
      simple = ContentTypedAction.async(block)
    )
  }

  case class SecureBlock(restrictionRef: String, set: Set[Permission]) extends SecureContext

  case class PartialSecureBlock(s: Set[Permission]) extends SecureContext

  case object NonSecureBlock extends SecureContext

  sealed trait Rule

  case object Create extends Rule

  case object Delete extends Rule

  case object All extends Rule

  case object Get extends Rule

  case object Update extends Rule
}

trait AbstractCRUDController[I, O <: UniqueEntity] extends Controller
  with JsonSerialisation[I, O]
  with SesameRdfSerialisation[O]
  with Filterable[O]
  with ModelConverter[I, O]
  with BaseNamespace
  with ContentTyped
  with Secured
  with SecureControllerContext
  with Consistent[I, O] {

  // POST /Ts
  def create(securedContext: SecureContext = contextFrom(Create)) = securedContext contentTypedAction { implicit request =>
    request.body.validate[I].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => {
        exists(success)(repository) match {
          case Some(id) =>
            Accepted(Json.obj(
              "status" -> "KO",
              "message" -> "model already exists",
              "id" -> id.toString
            ))
          case None =>
            val model = fromInput(success)

            repository.add[O](model) match {
              case Success(graph) =>
                Created(Json.toJson(model)).as(mimeType)
              case Failure(e) =>
                InternalServerError(Json.obj(
                  "status" -> "KO",
                  "errors" -> e.getMessage
                ))
            }
        }
      }
    )
  }

  // GET /Ts/:id
  def get(id: String, securedContext: SecureContext = contextFrom(Get)) = securedContext action { implicit request =>
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

  // GET /Ts/:id with deserialisation
  def getAtomic(id: String, securedContext: SecureContext = contextFrom(Get)) = securedContext action { implicit request =>
    import utils.Ops._
    import utils.Ops.MonadInstances.{tryM, optM}
    import utils.Ops.TraverseInstances._

    val uri = s"$namespace${request.uri}".replace("/atomic", "")

    repository.get[O](uri).flatPeek(atomize) match {
      case Success(s) =>
        s match {
          case Some(json) =>
            Ok(json).as(mimeType)
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
  def all(securedContext: SecureContext = contextFrom(All)) = securedContext action { implicit request =>
    repository.get[O] match {
      case Success(s) =>
        if (request.queryString.isEmpty)
          Ok(Json.toJson(s)).as(mimeType)
        else
          getWithFilter(request.queryString)(s) match {
            case Success(filtered) =>
                Ok(Json.toJson(filtered)).as(mimeType)
            case Failure(e) =>
              ServiceUnavailable(Json.obj(
                "status" -> "KO",
                "message" -> e.getMessage
              ))
          }
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  // GET /ts with optional queries and deserialisation
  def allAtomic(securedContext: SecureContext = contextFrom(All)) = securedContext action { implicit request =>
    def handle(t: Try[JsValue])(failure: JsObject => Result): Result = t match {
      case Success(json) =>
        Ok(json).as(mimeType)
      case Failure(e) =>
        failure(Json.obj(
        "status" -> "KO",
        "errors" -> e.getMessage
      ))
    }

    repository.get[O] match {
      case Success(os) =>
        if (request.queryString.isEmpty)
          handle(atomizeMany(os))(InternalServerError(_))
        else (getWithFilter(request.queryString)(_))
            .andThen(_.flatMap(atomizeMany))
            .andThen(handle(_)(ServiceUnavailable(_)))(os)
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  def update(id: String, securedContext: SecureContext = contextFrom(Update)) = securedContext contentTypedAction { implicit request =>
    val uri = s"$namespace${request.uri}"

    request.body.validate[I].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => {
        repository.get[O](uri) match {
          case Success(s) =>
            s match {
              case Some(entity) if compareModel(success, entity) =>
                  Accepted(Json.obj(
                    "status" -> "KO",
                    "message" -> "model already exists",
                    "id" -> id.toString
                  ))
              case Some(entity) =>
                val updated = fromInput(success, Some(entity.id))

                repository.update[O, UriGenerator[O]](updated) match {
                  case Success(graph) =>
                    Ok(Json.toJson(updated)).as(mimeType)
                  case Failure(e) =>
                    InternalServerError(Json.obj(
                      "status" -> "KO",
                      "errors" -> e.getMessage
                    ))
                }
              case None =>
                exists(success)(repository) match {
                  case Some(duplicate) =>
                    Accepted(Json.obj(
                      "status" -> "KO",
                      "message" -> "model already exists",
                      "id" -> duplicate.toString
                    ))
                  case None =>
                    val model = fromInput(success)

                    repository.add[O](model) match {
                      case Success(graph) =>
                        Created(Json.toJson(model)).as(mimeType)
                      case Failure(e) =>
                        InternalServerError(Json.obj(
                          "status" -> "KO",
                          "errors" -> e.getMessage
                        ))
                    }
                }
            }
          case Failure(e) =>
            InternalServerError(Json.obj(
              "status" -> "KO",
              "errors" -> e.getMessage
            ))
        }
      }
    )
  }

  def delete(id: String, securedContext: SecureContext = contextFrom(Delete)) = securedContext action { implicit request =>
    val uri = s"$namespace${request.uri}"

    import collection.JavaConversions._
    repository.delete(uri) match {
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