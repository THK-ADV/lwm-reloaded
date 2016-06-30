package controllers.crud

import java.util.UUID

import models.security.{Permission, Permissions}
import models.{UniqueEntity, UriGenerator}
import modules.store.BaseNamespace
import org.w3.banana.sesame.Sesame
import play.api.libs.iteratee.{Enumeratee, Enumerator}
import play.api.libs.json._
import play.api.mvc._
import services.{RoleService, SessionHandlingService}
import store.SesameRepository
import store.bind.Bindings
import store.bind.Descriptor.Descriptor
import store.sparql.Transitional
import utils.LwmActions._
import utils.LwmMimeType
import utils.Ops.MonadInstances.optM
import utils.Ops.NaturalTrasformations._

import scala.collection.Map
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object Return {
  def apply[X](elm: X): Return[X] = Continue(elm)
}

sealed trait Return[+X] {
  def map[Y](f: X => Y): Return[Y] = flatMap(f andThen Continue.apply)

  def flatMap[Y](f: X => Return[Y]): Return[Y] = this match {
    case Continue(a) => f(a)
    case Stop(r) => Stop(r)
  }


  def when[Y](p: X => Boolean,
              f: X => Return[Y])
             (fallback: => Result): Return[Y] = this match {
    case Continue(a) if p(a) => f(a)
    case Continue(_) => Stop(fallback)
    case Stop(r) => Stop(r)
  }

  def mapResult(f: X => Result): Result = this match {
    case Continue(x) => f(x)
    case Stop(r) => r
  }

}

case class Continue[+X](i: X) extends Return[X]

case class Stop(r: Result) extends Return[Nothing]


trait Stored {
  self: BaseNamespace =>
  type Rdf = Sesame
  val defaultBindings: Bindings[Rdf] = Bindings[Sesame](namespace)

  def repository: SesameRepository
}

trait RdfSerialisation[T <: UniqueEntity, A <: UniqueEntity] {
  implicit def descriptor: Descriptor[Sesame, T]

  implicit def descriptorAtom: Descriptor[Sesame, A]

  implicit def uriGenerator: UriGenerator[T]
}

trait JsonSerialisation[I, O, A] {

  implicit def setWrites[X](implicit w: Writes[X]): Writes[Set[X]] = Writes.set[X]

  implicit def reads: Reads[I]

  implicit def writes: Writes[O]

  implicit def writesAtom: Writes[A]
}

trait Filterable[O] {
  protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[O]): Try[Set[O]]
}

trait ModelConverter[I, O] {
  protected def fromInput(input: I, existing: Option[O] = None): O
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

trait Chunked {
  final def chunk[A](data: Set[A])(implicit writes: Writes[A]): Enumerator[JsValue] = {
    Enumerator.enumerate(data) &> Enumeratee.map[A](writes.writes)
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

trait AbstractCRUDController[I, O <: UniqueEntity, A <: UniqueEntity] extends Controller
  with JsonSerialisation[I, O, A]
  with RdfSerialisation[O, A]
  with Filterable[O]
  with ModelConverter[I, O]
  with BaseNamespace
  with ContentTyped
  with Secured
  with SessionChecking
  with SecureControllerContext
  with Consistent[I, O]
  with Chunked
  with Stored
  with Basic[I, O, A] {

  def create(secureContext: SecureContext = contextFrom(Create)) = secureContext contentTypedAction { request =>
    validate(request)
      .flatMap(existence)
      .flatMap(add)
      .mapResult(o => Created(Json.toJson(o)).as(mimeType))
  }

  def createAtomic(secureContext: SecureContext = contextFrom(Create)) = secureContext contentTypedAction { request =>
    validate(request)
      .flatMap(existence)
      .flatMap(add)
      .flatMap { o =>
        val uri = s"$namespace${request.uri}".replace("/atomic", "") + s"/${o.id}"
        retrieve[A](uri)
      }
      .mapResult(a => Created(Json.toJson(a)).as(mimeType))
  }

  def get(id: String, securedContext: SecureContext = contextFrom(Get)) = securedContext action { request =>
    val uri = s"$namespace${request.uri}"
    retrieve[O](uri)
      .mapResult(o => Ok(Json.toJson(o)).as(mimeType))
  }

  def getAtomic(id: String, securedContext: SecureContext = contextFrom(Get)) = securedContext action { request =>
    val uri = s"$namespace${request.uri}".replace("/atomic", "")

    retrieve[A](uri)
      .mapResult(a => Ok(Json.toJson(a)).as(mimeType))
  }

  def update(id: String, secureContext: SecureContext = contextFrom(Update)) = secureContext contentTypedAction { request =>
    val uri = s"$namespace${request.uri}".replaceAll("/atomic", "")

    validate(request)
      .flatMap(input => replace(uri, input))
      .mapResult(o => Ok(Json.toJson(o)).as(mimeType))
  }

  def updateAtomic(id: String, securedContext: SecureContext = contextFrom(Update)) = securedContext contentTypedAction { request =>
    val uri = s"$namespace${request.uri}".replaceAll("/atomic", "")

    validate(request)
      .flatMap(input => replace(uri, input))
      .flatMap(i => retrieve[A](uri))
      .mapResult(a => Ok(Json.toJson(a)).as(mimeType))
  }

  def all(securedContext: SecureContext = contextFrom(GetAll)) = securedContext action { request =>
    retrieveAll[O]
      .flatMap(filtered(request))
      .mapResult(os => Ok(Json.toJson(os)).as(mimeType))
  }

  def allAtomic(securedContext: SecureContext = contextFrom(GetAll)) = securedContext action { request =>
    retrieveAll[A]
      .flatMap(filtered2(request, coatomic))
      .mapResult(as => Ok(Json.toJson(as)).as(mimeType))
  }

  def delete(id: String, securedContext: SecureContext = contextFrom(Delete)) = securedContext action { implicit request =>
    val uri = s"$namespace${request.uri}"

    remove[O](uri)
      .mapResult(result => result)
  }

  def header = Action { implicit request =>
    NoContent.as(mimeType)
  }

  protected def coatomic(atom: A): O
}

trait Filtered[O <: UniqueEntity, A <: UniqueEntity] {
  self: Controller with Filterable[O] =>

  def filtered[R](req: Request[R])(os: Set[O]): Return[Set[O]] = {
    if (req.queryString.isEmpty) Continue(os)
    else getWithFilter(req.queryString)(os) match {
      case Success(fs) => Continue(fs)
      case Failure(e) =>
        Stop(ServiceUnavailable(Json.obj(
          "status" -> "KO",
          "message" -> e.getMessage
        )))
    }
  }

  def filtered2[R](req: Request[R], f: A => O)(as: Set[A]): Return[Set[A]] = {
    filtered(req)(as map f)
      .map(fos => as filter (x => fos exists (_.id == x.id)))
  }
}

trait Retrieved[O <: UniqueEntity, A <: UniqueEntity] {
  self: Controller with Stored =>

  def optional[X](item: Try[Option[X]]): Return[X] = item match {
    case Success(Some(a)) => Continue(a)
    case Success(None) => Stop(
      NotFound(Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )))
    case Failure(e) => Stop(
      InternalServerError(Json.obj(
        "status" -> "KO",
        "errors" -> e.getMessage
      )))
  }

  def retrieveLots[X <: UniqueEntity](uris: TraversableOnce[String])(implicit descriptor: Descriptor[Rdf, X]): Return[Set[X]] = {
    repository.getMany[X](uris) match {
      case Success(set) => Continue(set)
      case Failure(e) => Stop(
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }

  def retrieve[X <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, X]): Return[X] = {
    (optional[X] _ compose repository.get[X]) (uri)
  }

  def retrieveAll[X <: UniqueEntity](implicit descriptor: Descriptor[Rdf, X]): Return[Set[X]] = {
    repository.getAll[X] match {
      case Success(a) => Continue(a)
      case Failure(e) => Stop(
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }

  def queried[F[_], X](t: Transitional[F, X]): Return[F[X]] = {
    t.run match {
      case Success(a) => Continue(a)
      case Failure(e) => Stop(
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
      )
    }
  }
}

trait Added[I, O <: UniqueEntity, A <: UniqueEntity] {
  self: Controller
    with Stored
    with RdfSerialisation[O, A]
    with JsonSerialisation[I, O, A] =>

  def validate(request: Request[JsValue]) = {
    request.body.validate[I].fold(
      errors => {
        Stop(BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        )))
      },
      success => Continue(success)
    )
  }

  def add(output: O): Return[O] = {
    repository.add[O](output) match {
      case Success(_) => Continue(output)
      case Failure(e) =>
        Stop(InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }

  def addLots(output: TraversableOnce[O]): Return[List[O]] = {
    repository.addMany[O](output) match {
      case Success(_) => Continue(output.toList)
      case Failure(e) =>
        Stop(InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }
}

trait Updated[I, O <: UniqueEntity, A <: UniqueEntity] {
  self: Controller
    with Stored
    with Added[I, O, A]
    with RdfSerialisation[O, A]
    with ModelConverter[I, O]
    with Consistent[I, O] =>

  def compare(i: I, o: O): Return[(I, O)] = {
    if (compareModel(i, o))
      Stop(Accepted(Json.obj(
        "status" -> "KO",
        "message" -> "model already exists",
        "id" -> o.id.toString)))
    else Continue((i, o))
  }

  def overwrite0(item: O): Return[O] = {
    repository.update[O, UriGenerator[O]](item) match {
      case Success(_) => Continue(item)
      case Failure(e) => Stop(
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }

  def existence(input: I): Return[O] = exists(input)(repository) match {
    case Success(Some(duplicate)) =>
      Stop(Accepted(Json.obj(
        "status" -> "KO",
        "message" -> "model already exists",
        "id" -> duplicate.toString
      )))
    case Success(None) =>
      Continue(fromInput(input))
    case Failure(e) =>
      Stop(InternalServerError(Json.obj(
        "status" -> "KO",
        "errors" -> e.getMessage
      )))
  }

  def replace(uri: String, input: I) = {
    repository.get[O](uri) match {
      case Success(Some(o)) if compareModel(input, o) =>
        Stop(Accepted(Json.obj(
          "status" -> "KO",
          "message" -> "model already exists",
          "id" -> o.id.toString)))
      case Success(Some(o)) => overwrite(input, o)
      case Success(None) => existence(input) flatMap add
      case Failure(e) =>
        Stop(InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }

  def overwrite(input: I, existing: O): Return[O] = {
    val updated = fromInput(input, Some(existing))
    repository.update[O, UriGenerator[O]](updated) match {
      case Success(_) => Continue(updated)
      case Failure(e) => Stop(
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }
}

trait Removed[O <: UniqueEntity, A <: UniqueEntity] {
  self: Controller with
    Stored with
    RdfSerialisation[O, A] =>

  def remove[X <: UniqueEntity](uri: String)(implicit desc: Descriptor[Rdf, X]): Return[Result] = {
    repository.delete[X](uri) match {
      case Success(_) =>
        Stop(
          Ok(Json.obj(
            "status" -> "OK"
          )))
      case Failure(e) =>
        Stop(
          InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> e.getMessage
          )))
    }
  }

}

trait Basic[I, O <: UniqueEntity, A <: UniqueEntity] extends Added[I, O, A] with Updated[I, O, A] with Removed[O, A] with Retrieved[O, A] with Filtered[O, A] {
  self: Controller
    with Stored
    with RdfSerialisation[O, A]
    with ModelConverter[I, O]
    with Consistent[I, O]
    with JsonSerialisation[I, O, A]
    with Filterable[O] =>
}