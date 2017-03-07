package controllers

import java.util.UUID

import models.{SesamePermission, Permissions, UniqueEntity, UriGenerator}
import modules.BaseNamespace
import org.w3.banana.sesame.Sesame
import play.api.libs.iteratee.{Enumeratee, Enumerator}
import play.api.libs.json._
import play.api.mvc._
import services.{RoleService, SessionHandlingService}
import store.bind.Bindings
import store.bind.Descriptor.Descriptor
import store.sparql.Transitional
import store.{Namespace, SesameRepository}
import utils.LwmActions._
import utils.Ops.MonadInstances.optM
import utils.{Attempt, Continue, LwmMimeType, Return}

import scala.collection.Map
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

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
  implicit def sessionService: SessionHandlingService
}

trait Consistent[I, O <: UniqueEntity] {

  import store.sparql.{Clause, NoneClause, SelectClause}

  final def exists(input: I)(repository: SesameRepository)(implicit descriptor: Descriptor[repository.Rdf, O]): Try[Option[O]] = {
    import utils.Ops.NaturalTrasformations._
    import utils.Ops.TraverseInstances.travO

    existsQuery(input) match {
      case select@SelectClause(_, _) =>
        repository.prepareQuery(select).
          select(_.get("s")).
          changeTo(_.headOption).
          map(_.stringValue)(optM).
          request(repository.get[O]).
          run

      case _ => Success(None)
    }
  }

  protected def existsQuery(input: I): Clause = NoneClause

  protected def compareModel(input: I, output: O): Boolean
}

trait RequestRebase[O <: UniqueEntity] {

  final def rebase[A](implicit request: Request[A], uriGenerator: UriGenerator[O]): Request[A] = {
    rebase0(None)
  }

  final def rebase[A](id: String)(implicit request: Request[A], uriGenerator: UriGenerator[O]): Request[A] = {
    rebase0(Some(UUID.fromString(id)))
  }

  final def rebase[A](query: (String, Seq[String])*)(implicit request: Request[A], uriGenerator: UriGenerator[O]): Request[A] = {
    rebase0(None, query)
  }

  private def rebase0[A](id: Option[UUID], query: Seq[(String, Seq[String])] = Seq())(implicit request: Request[A], uriGenerator: UriGenerator[O]): Request[A] = {
    val uri = id.fold(uriGenerator.generateBase)(uuid => uriGenerator.generateBase(uuid))
    val queryString = query.foldLeft(request.queryString)(_ + _)
    val headers = request.copy(request.id, request.tags, uri, request.path, request.method, request.version, queryString)
    Request(headers, request.body)
  }

  final def rebase[A](id: String, query: (String, Seq[String])*)(implicit request: Request[A], uriGenerator: UriGenerator[O]): Request[A] = {
    rebase0(Some(UUID.fromString(id)), query)
  }

  final def asUri[A](ns: Namespace, request: Request[A]): String = {
    s"$ns${request.uri}".replaceAll("/atomic", "")
  }
}

trait Chunked {
  final def chunk[A](data: Set[A])(implicit writes: Writes[A]): Enumerator[JsValue] = {
    Enumerator.enumerate(data) &> Enumeratee.map[A](writes.writes)
  }
}

trait ContentTyped {
  implicit def mimeType: LwmMimeType
}

trait Secured {
  implicit def roleService: RoleService
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

  //to be specialized
  protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Permissions.prime)
  }

  //to be specialized
  protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Permissions.prime)
  }

  sealed trait Rule

  trait SecureContext {

    def action(block: Request[AnyContent] => Result): Action[AnyContent] = apply[AnyContent](
      restricted = (opt, perms) => SecureAction((opt, perms))(block),
      simple = Action(block)
    )

    def contentTypedAction(block: Request[JsValue] => Result): Action[JsValue] = apply[JsValue](
      restricted = (opt, perms) => SecureContentTypedAction((opt, perms))(block),
      simple = ContentTypedAction(block)
    )

    def apply[A](restricted: (Option[UUID], SesamePermission) => Action[A], simple: => Action[A]) = this match {
      case SecureBlock(id, permission) => restricted(Some(UUID.fromString(id)), permission)
      case PartialSecureBlock(permission) => restricted(None, permission)
      case NonSecureBlock => simple()
    }

    def asyncAction(block: Request[AnyContent] => Future[Result]): Action[AnyContent] = apply[AnyContent](
      restricted = (opt, perms) => SecureAction.async((opt, perms))(block),
      simple = Action.async(block)
    )

    def asyncContentTypedAction(block: Request[JsValue] => Future[Result]): Action[JsValue] = apply[JsValue](
      restricted = (opt, perms) => SecureContentTypedAction.async((opt, perms))(block),
      simple = ContentTypedAction.async(block)
    )
  }

  case class SecureBlock(restrictionRef: String, permission: SesamePermission) extends SecureContext

  case class PartialSecureBlock(permission: SesamePermission) extends SecureContext

  case object Create extends Rule

  case object Delete extends Rule

  case object GetAll extends Rule

  case object Get extends Rule

  case object Update extends Rule

  case object NonSecureBlock extends SecureContext

}

trait PostgresResult { self: Controller =>

  private def internalServerError(message: String) = InternalServerError(Json.obj("status" -> "KO", "message" -> message))
  private def notFound(element: String) = NotFound(Json.obj("status" -> "KO", "message" -> s"No such element for $element"))

  implicit class SequenceResult[A](val future: Future[Seq[A]]) {
    def jsonResult(implicit writes: Writes[A]) = future.map(a => Ok(Json.toJson(a))).recover {
      case NonFatal(e) => internalServerError(e.getMessage)
    }
  }

  implicit class OptionResult[A](val future: Future[Option[A]]) {
    def jsonResult(idForMessage: String)(implicit writes: Writes[A]) = future.map { maybeA =>
      maybeA.fold(notFound(idForMessage))(a => Ok(Json.toJson(a)) )
    }.recover {
      case NonFatal(e) => internalServerError(e.getMessage)
    }
  }

  implicit class CreatedResult[A](val future: Future[A]) {
    def jsonResult(implicit writes: Writes[A]) = future.map(a => Created(Json.toJson(a))).recover {
      case NonFatal(e) => internalServerError(e.getMessage)
    }
  }

  lazy val atomicAttribute = "atomic"

  type QueryString = Map[String, Seq[String]]

  final def extractAtomic(queryString: QueryString): (QueryString, Boolean) = {
    queryString.find(_._1 == `atomicAttribute`) match {
      case Some(q) =>
        val atomic = q._2.headOption.flatMap(s => Try(s.toBoolean).toOption).fold(false)(_ == true)
        val remaining = queryString - `atomicAttribute`

        (remaining, atomic)
      case None =>
        (queryString, false)
    }
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
  with RequestRebase[O]
  with Chunked
  with Stored
  with Basic[I, O, A] {

  def create(secureContext: SecureContext = contextFrom(Create)) = secureContext contentTypedAction { request =>
    validateInput(request)
      .flatMap(existence)
      .flatMap(add)
      .mapResult(o => Created(Json.toJson(o)).as(mimeType))
  }

  def createAtomic(secureContext: SecureContext = contextFrom(Create)) = secureContext contentTypedAction { request =>
    validateInput(request)
      .flatMap(existence)
      .flatMap(add)
      .flatMap { o =>
        val uri = uriGenerator.generateUri(o)(namespace)
        retrieve[A](uri)
      }
      .mapResult(a => Created(Json.toJson(a)).as(mimeType))
  }

  def get(id: String, securedContext: SecureContext = contextFrom(Get)) = securedContext action { request =>
    val uri = asUri(namespace, request)
    retrieve[O](uri)
      .mapResult(o => Ok(Json.toJson(o)).as(mimeType))
  }

  def getAtomic(id: String, securedContext: SecureContext = contextFrom(Get)) = securedContext action { request =>
    val uri = asUri(namespace, request)
    retrieve[A](uri)
      .mapResult(a => Ok(Json.toJson(a)).as(mimeType))
  }

  def update(id: String, secureContext: SecureContext = contextFrom(Update)) = secureContext contentTypedAction { request =>
    val uri = asUri(namespace, request)
    validateInput(request)
      .flatMap(input => replace(uri, input))
      .mapResult(o => Ok(Json.toJson(o)).as(mimeType))
  }

  def updateAtomic(id: String, securedContext: SecureContext = contextFrom(Update)) = securedContext contentTypedAction { request =>
    val uri = asUri(namespace, request)
    validateInput(request)
      .flatMap(input => replace(uri, input))
      .flatMap(i => retrieve[A](uri))
      .mapResult(a => Ok(Json.toJson(a)).as(mimeType))
  }

  def all(securedContext: SecureContext = contextFrom(GetAll)) = securedContext action { request =>
    retrieveAll[O]
      .flatMap(filter(request))
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def allAtomic(securedContext: SecureContext = contextFrom(GetAll)) = securedContext action { request =>
    retrieveAll[A]
      .flatMap(filter2(request, coAtomic))
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def filteredOnly(secureContext: SecureContext = contextFrom(GetAll)) = secureContext action { request =>
    filter(request)(Set.empty[O])
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def filteredAtomicOnly(secureContext: SecureContext = contextFrom(GetAll)) = secureContext action { request =>
    filter(request)(Set.empty[O])
      .map(_.map(o => uriGenerator.generateUri(o)(namespace)))
      .flatMap(retrieveLots[A])
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def delete(id: String, securedContext: SecureContext = contextFrom(Delete)) = securedContext action { request =>
    val uri = asUri(namespace, request)
    invalidate[O](uri)
      .mapResult(_ => Ok(Json.obj("status" -> "OK")))
  }

  def header = Action { implicit request =>
    NoContent.as(mimeType)
  }

  protected def coAtomic(atom: A): O
}

trait Filtered[O <: UniqueEntity, A <: UniqueEntity] {
  self: Controller with Filterable[O] =>

  def filter2[R](req: Request[R], f: A => O)(as: Set[A]): Attempt[Set[A]] = {
    filter(req)(as map f)
      .map(fos => as filter (x => fos exists (_.id == x.id)))
  }

  def filter[R](req: Request[R])(os: Set[O]): Attempt[Set[O]] = {
    if (req.queryString.isEmpty)
      Continue(os)
    else
      getWithFilter(req.queryString)(os) match {
        case Success(fs) => Continue(fs)
        case Failure(e) =>
          Return(ServiceUnavailable(Json.obj(
            "status" -> "KO",
            "message" -> e.getMessage
          )))
      }
  }
}

trait Retrieved[O <: UniqueEntity, A <: UniqueEntity] {
  self: Controller with Stored =>

  def retrieveLots[X <: UniqueEntity](uris: TraversableOnce[String])(implicit descriptor: Descriptor[Rdf, X]): Attempt[Set[X]] = {
    repository.getMany[X](uris) match {
      case Success(set) => Continue(set)
      case Failure(e) => Return(
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }

  def retrieve[X <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, X]): Attempt[X] = {
    (optional[X] _ compose repository.get[X]) (uri)
  }

  def optional[X](tryOpt: Try[Option[X]]): Attempt[X] = tryOpt match {
    case Success(s) => optional2(s)
    case Failure(e) => Return(
      InternalServerError(Json.obj(
        "status" -> "KO",
        "errors" -> e.getMessage
      )))
  }

  def optional2[X](opt: Option[X]): Attempt[X] = opt match {
    case Some(s) => Continue(s)
    case None => Return(
      NotFound(Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )))
  }

  def attempt[X](item: Try[X]): Attempt[X] = item match {
    case Success(s) => Continue(s)
    case Failure(e) => Return(
      InternalServerError(Json.obj(
        "status" -> "KO",
        "errors" -> e.getMessage
      )))
  }

  def retrieveAll[X <: UniqueEntity](implicit descriptor: Descriptor[Rdf, X]): Attempt[Set[X]] = {
    repository.getAll[X] match {
      case Success(a) => Continue(a)
      case Failure(e) => Return(
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }

  def queried[F[_], X](t: Transitional[F, X]): Attempt[F[X]] = {
    t.run match {
      case Success(a) => Continue(a)
      case Failure(e) => Return(
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

  def validateInput(request: Request[JsValue]) = {
    request.body.validate[I].fold(
      errors => {
        Return(BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        )))
      },
      success => Continue(success)
    )
  }

  def validate[X](request: Request[JsValue])(implicit reads: Reads[X]) = {
    request.body.validate[X].fold(
      errors => {
        Return(BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        )))
      },
      success => Continue(success)
    )
  }

  def add(output: O): Attempt[O] = {
    repository.add[O](output) match {
      case Success(_) => Continue(output)
      case Failure(e) =>
        Return(InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }

  def addLots(output: TraversableOnce[O]): Attempt[List[O]] = {
    repository.addMany[O](output) match {
      case Success(_) => Continue(output.toList)
      case Failure(e) =>
        Return(InternalServerError(Json.obj(
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

  def compare(i: I, o: O): Attempt[(I, O)] = {
    if (compareModel(i, o))
      Return(Accepted(Json.obj(
        "status" -> "KO",
        "message" -> "model already exists",
        "id" -> o.id.toString)))
    else Continue((i, o))
  }

  def overwrite0(item: O): Attempt[O] = {
    repository.update[O, UriGenerator[O]](item) match {
      case Success(_) => Continue(item)
      case Failure(e) => Return(
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }

  def replace(uri: String, input: I) = {
    repository.get[O](uri) match {
      case Success(Some(o)) if compareModel(input, o) =>
        Return(Accepted(Json.obj(
          "status" -> "KO",
          "message" -> "model already exists",
          "id" -> o.id.toString)))
      case Success(Some(o)) => overwrite(input, o)
      case Success(None) => existence(input) flatMap add
      case Failure(e) =>
        Return(InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }

  def existence(input: I): Attempt[O] = exists(input)(repository) match {
    case Success(Some(duplicate)) =>
      Return(Accepted(Json.obj(
        "status" -> "KO",
        "message" -> "model already exists",
        "id" -> duplicate.id.toString
      )))
    case Success(None) =>
      Continue(fromInput(input))
    case Failure(e) =>
      Return(InternalServerError(Json.obj(
        "status" -> "KO",
        "errors" -> e.getMessage
      )))
  }

  def overwrite(input: I, existing: O): Attempt[O] = {
    val updated = fromInput(input, Some(existing))
    repository.update[O, UriGenerator[O]](updated) match {
      case Success(_) => Continue(updated)
      case Failure(e) => Return(
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }
}

trait Removed {
  self: Controller with
    Stored =>

  def remove[X <: UniqueEntity](uri: String)(implicit desc: Descriptor[Rdf, X]): Attempt[Unit] = {
    repository.delete[X](uri) match {
      case Success(_) => Continue(())
      case Failure(e) =>
        Return(
          InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> e.getMessage
          )))
    }
  }

  def removeLots[X <: UniqueEntity](uris: TraversableOnce[String])(implicit desc: Descriptor[Rdf, X]): Attempt[Set[Unit]] = {
    repository.deleteMany[X](uris) match {
      case Success(s) => Continue(s)
      case Failure(e) =>
        Return(
          InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> e.getMessage
          )))
    }
  }

  def invalidate[X <: UniqueEntity](uri: String)(implicit desc: Descriptor[Rdf, X]): Attempt[Unit] = {
    repository.invalidate[X](uri) match {
      case Success(_) => Continue(())
      case Failure(e) =>
        Return(
          InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> e.getMessage
          )))
    }
  }

}

trait Basic[I, O <: UniqueEntity, A <: UniqueEntity] extends Added[I, O, A] with Updated[I, O, A] with Removed with Retrieved[O, A] with Filtered[O, A] {
  self: Controller
    with Stored
    with RdfSerialisation[O, A]
    with ModelConverter[I, O]
    with Consistent[I, O]
    with JsonSerialisation[I, O, A]
    with Filterable[O] =>
}