package controllers

import java.util.UUID

import controllers.helper._
import dao.AbstractDao
import dao.helper.{Retrieved, TableFilter}
import database.{LabworkIdTable, UniqueTable}
import javax.inject.Inject
import models.Role.God
import models.{UniqueDbEntity, UniqueEntity}
import play.api.libs.json._
import play.api.mvc._
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Rep

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

abstract class AbstractCRUDController[Protocol, T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity] @Inject()(cc: ControllerComponents)
  extends AbstractController(cc)
    with Secured
    with SecureControllerContext
    with ResultOps
    with AttributeFilter
    with RequestOps
    with TableFilter[T] {

  import utils.Ops.unwrapTrys

  import scala.concurrent.ExecutionContext.Implicits.global

  protected implicit def writes: Writes[LwmModel]

  protected implicit def reads: Reads[Protocol]

  implicit def listReads[R](implicit r: Reads[R]): Reads[List[R]] = Reads.list[R]

  protected def abstractDao: AbstractDao[T, DbModel, LwmModel]

  protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = Failure(new Throwable(""))

  protected def toDbModel(protocol: Protocol, existingId: Option[UUID]): DbModel

  protected def toLwmModel(dbModels: TraversableOnce[DbModel]): Traversable[LwmModel] = dbModels.map(_.toUniqueEntity.asInstanceOf[LwmModel]).toTraversable

  final protected def parseJson[R](request: Request[AnyContent])(implicit reads: Reads[R]): Try[R] = unwrap(request).flatMap(js => validate(js)(reads))

  final protected def parseJsonArray[R](request: Request[AnyContent])(implicit reads: Reads[List[R]]): Try[List[R]] = unwrap(request).flatMap(js => validate(js)(reads))

  final protected def forbidden(): PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(List(God))
  }

  private def validate[A](json: JsValue)(implicit reads: Reads[A]) = json.validate[A].fold[Try[A]](
    errors => Failure(new Throwable(JsError.toJson(errors).toString)),
    success => Success(success)
  )

  private def unwrap(request: Request[AnyContent]) = request.body.asJson match {
    case Some(json) => Success(json)
    case None => Failure(new Throwable("no json body"))
  }

  def create(secureContext: SecureContext = contextFrom(Create)) = secureContext asyncAction { request =>
    val atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic

    (for {
      protocols <- Future.fromTry(parseJsonArray(request)(listReads))
      dbModels = protocols.map(p => toDbModel(p, None))
      partialCreated <- abstractDao.createManyPartial(dbModels)
      (succeeded, failed) = unwrapTrys(partialCreated)
      lwmModel <- if (atomic)
        abstractDao.getMany(succeeded.map(_.id), atomic)
      else
        Future.successful(toLwmModel(succeeded))
    } yield (toLwmModel(dbModels), lwmModel, failed)).jsonResult
  }

  def update(id: String, secureContext: SecureContext = contextFrom(Update)) = secureContext asyncAction { request =>
    val uuid = UUID.fromString(id)
    val atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic

    (for {
      protocol <- Future.fromTry(parseJson(request)(reads))
      dbModel = toDbModel(protocol, Some(uuid))
      updated <- abstractDao.update(dbModel)
      lwmModel <- if (atomic)
        abstractDao.getSingle(uuid, atomic).map(_.get)
      else
        Future.successful(updated.toUniqueEntity.asInstanceOf[LwmModel])
    } yield lwmModel).updated
  }

  def delete(id: String, secureContext: SecureContext = contextFrom(Delete)) = secureContext asyncAction { _ =>
    delete0(UUID.fromString(id))
  }

  protected def delete0(uuid: UUID): Future[Result] = {
    abstractDao.delete(uuid).deleted
  }

  def all(secureContext: SecureContext = contextFrom(GetAll)) = secureContext asyncAction { request =>
    import utils.Ops.MonadInstances.tryM
    import utils.Ops._

    val (queryString, defaults) = extractAttributes(request.queryString)
    val filter = makeTableFilter(queryString).sequence

    (for {
      filter <- Future.fromTry(filter)
      results <- abstractDao.get(filter, defaults.atomic, defaults.valid, defaults.lastModified)
    } yield results).jsonResult
  }

  private def makeTableFilter(queryString: QueryString): List[Try[TableFilterPredicate]] = {
    (for {
      (attribute, values) <- queryString if values.nonEmpty
    } yield makeTableFilter(attribute, values.head)).toList
  }

  def get(id: String, secureContext: SecureContext = contextFrom(Get)) = secureContext asyncAction { request =>
    val atomic = extractAttributes(request.queryString)._2.atomic
    val uuid = UUID.fromString(id)
    abstractDao.getSingle(uuid, atomic).jsonResult(id)
  }

  implicit class AbstractTableFilter(string: String) {

    private def labworkFilter[A <: LabworkIdTable](labwork: UUID): A => Rep[Boolean] = _.labwork === labwork

    private def courseFilter[A <: LabworkIdTable](course: UUID): A => Rep[Boolean] = _.memberOfCourse(course)

    def uuid: Try[UUID] = Try(UUID.fromString(string))

    def uuidF: Future[UUID] = Future.fromTry(string.uuid)

    def makeCourseFilter[A <: LabworkIdTable]: Try[A => Rep[Boolean]] = string.uuid map courseFilter

    def makeLabworkFilter[A <: LabworkIdTable]: Try[A => Rep[Boolean]] = string.uuid map labworkFilter
  }

}