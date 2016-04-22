package controllers.crud

import java.util.UUID

import models.security.{Permission, Permissions}
import models.{UniqueEntity, UriGenerator}
import modules.store.BaseNamespace
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.iteratee.{Enumeratee, Enumerator, Input}
import play.api.libs.json._
import play.api.mvc._
import services.{RoleService, SessionHandlingService}
import store.SesameRepository
import store.bind.Bindings
import utils.LwmActions._
import utils.LwmMimeType
import utils.Ops.MonadInstances.optM
import utils.Ops.NaturalTrasformations._

import scala.collection.Map
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

trait SesameRdfSerialisation[T <: UniqueEntity] {
  self: BaseNamespace =>

  val defaultBindings: Bindings[Sesame] = Bindings[Sesame](namespace)

  def repository: SesameRepository

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
  protected def fromInput(input: I, existing: Option[O] = None): O
}

trait Atomic[O] {
  import utils.Ops._
  import utils.Ops.MonadInstances._

  protected def atomize(output: O): Try[Option[JsValue]]

  protected def atomizeMany(output: Set[O]): Try[JsValue] = output.foldLeft(Try(Option(JsArray()))) { (T, model) =>
    T.bipeek(atomize(model))(_ :+ _)
  } map {
    case Some(jsArray) => jsArray
    case None => JsArray()
  }
}

trait SessionChecking {
  implicit val sessionService: SessionHandlingService
}

trait Consistent[I, O] {

  import store.sparql.select._
  import store.sparql.{NoneClause, Clause, SelectClause}

  final def exists(input: I)(repository: SesameRepository): Try[Option[UUID]] = {
    val (clause, key) = existsQuery(input)

    clause match {
      case select@SelectClause(_, _) =>
        repository.prepareQuery(select).
          select(_.get(key.v)).
          changeTo(_.headOption).
          map(value => UUID.fromString(value.stringValue())).
          run

      case _ => Success(None)
    }
  }

  protected def existsQuery(input: I): (Clause, Var) = (NoneClause, v(""))

  protected def compareModel(input: I, output: O): Boolean
}

trait Chunkable[I] { self: Atomic[I] =>

  final def chunkAtoms(data: Set[I]): Enumerator[JsValue] = chunkWith(data)(atomize)
  final def chunkSimple(data: Set[I])(implicit writes: Writes[I]): Enumerator[JsValue] = chunkWith(data)(i => Success(Some(Json.toJson(i))))
  final def chunkSimpleJson(data: Set[I])(toJson: I => Try[Option[JsValue]]): Enumerator[JsValue] = chunkWith(data)(toJson)

  private final def chunkWith[O](data: Set[I])(f: I => Try[Option[O]]): Enumerator[O] = {
    val result = Enumeratee.map[I](f)
    val transfer = Enumeratee.mapInput[Try[Option[O]]] {
      case Input.El(Success(Some(out))) => Input.El[O](out)
      case Input.El(Success(None)) => Input.Empty
      case _ => Input.EOF
    }

    Enumerator.enumerate(data) &> result &> transfer
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
  self: Secured with SessionChecking with ContentTyped =>

  sealed trait Rule

  case object Create extends Rule

  case object Delete extends Rule

  case object GetAll extends Rule

  case object Get extends Rule

  case object Update extends Rule

  trait SecureContext {

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

    def apply[A](restricted: (Option[UUID], Permission) => Action[A], simple: => Action[A]) = this match {
      case SecureBlock(id, permission) => restricted(Some(UUID.fromString(id)), permission)
      case PartialSecureBlock(permission) => restricted(None, permission)
      case NonSecureBlock => simple()
    }
  }

  case class SecureBlock(restrictionRef: String, permission: Permission) extends SecureContext

  case class PartialSecureBlock(permission: Permission) extends SecureContext

  case object NonSecureBlock extends SecureContext

  //to be specialized
  protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Permissions.prime)
  }

  //to be specialized
  protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Permissions.prime)
  }
}

trait AbstractCRUDController[I, O <: UniqueEntity] extends Controller
  with JsonSerialisation[I, O]
  with SesameRdfSerialisation[O]
  with Filterable[O]
  with ModelConverter[I, O]
  with BaseNamespace
  with ContentTyped
  with Secured
  with SessionChecking
  with SecureControllerContext
  with Consistent[I, O]
  with Chunkable[O]
  with Atomic[O] {


  // POST /Ts
  def create(securedContext: SecureContext = contextFrom(Create)) = createWith(securedContext) { output =>
    repository.add[O](output).map(_ => Created(Json.toJson(output)).as(mimeType))
  }

  // POST /Ts with deserialisation
  def createAtomic(secureContext: SecureContext = contextFrom(Create)) = createWith(secureContext) { output =>
    repository.add[O](output).flatMap(_ => atomize(output)).map {
      case Some(json) =>
        Created(json).as(mimeType)
      case None =>
        NotFound(Json.obj(
          "status" -> "KO",
          "message" -> "No such element..."
        ))
    }
  }

  private def createWith(securedContext: SecureContext)(f: O => Try[Result]) = securedContext contentTypedAction { implicit request =>
    request.body.validate[I].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => existenceOf(success)(f)
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
  def all(securedContext: SecureContext = contextFrom(GetAll)) = securedContext action { implicit request =>
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
  def allAtomic(securedContext: SecureContext = contextFrom(GetAll)) = securedContext action { implicit request =>
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

  // PUT /Ts/:id
  def update(id: String, secureContext: SecureContext = contextFrom(Update)) = updateWith(id, secureContext) { output =>
    Success(Ok(Json.toJson(output)).as(mimeType))
  } { output =>
    repository.add[O](output).map(_ => Created(Json.toJson(output)).as(mimeType))
  }

  // PUT /Ts/:id with deserialisation
  def updateAtomic(id: String, securedContext: SecureContext = contextFrom(Update)) = updateWith(id, securedContext) { output =>
    atomize(output).map {
      case Some(json) =>
        Ok(json).as(mimeType)
      case None =>
        NotFound(Json.obj(
          "status" -> "KO",
          "message" -> "No such element..."
        ))
    }
  } { output =>
    repository.add[O](output).flatMap(_ => atomize(output)).map {
      case Some(json) =>
        Created(json).as(mimeType)
      case None =>
        NotFound(Json.obj(
          "status" -> "KO",
          "message" -> "No such element..."
        ))
    }
  }

  private def updateWith(id: String, securedContext: SecureContext)
                        (updatef: O => Try[Result])
                        (addf: O => Try[Result]) = securedContext contentTypedAction { implicit request =>
    val uri = s"$namespace${request.uri}".replaceAll("/atomic", "")

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
                val updated = fromInput(success, Some(entity))

                repository.update[O, UriGenerator[O]](updated).flatMap(_ => updatef(updated)) match {
                  case Success(result) => result
                  case Failure(e) =>
                    InternalServerError(Json.obj(
                      "status" -> "KO",
                      "errors" -> e.getMessage
                    ))
                }
              case None => existenceOf(success)(addf)
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

  protected def existenceOf(input: I)(f: O => Try[Result]) = exists(input)(repository) match {
    case Success(Some(duplicate)) =>
      Accepted(Json.obj(
        "status" -> "KO",
        "message" -> "model already exists",
        "id" -> duplicate.toString
      ))
    case Success(None) =>
      f(fromInput(input)) match {
        case Success(result) => result
        case Failure(e) =>
          InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> e.getMessage
          ))
      }
    case Failure(e) =>
      InternalServerError(Json.obj(
        "status" -> "KO",
        "errors" -> e.getMessage
      ))
  }


  def delete(id: String, securedContext: SecureContext = contextFrom(Delete)) = securedContext action { implicit request =>
    val uri = s"$namespace${request.uri}"

    repository.deleteCascading(uri) match {
      case Success(s) =>
        Ok(Json.obj(
          "status" -> "OK",
          "deleted" -> s
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