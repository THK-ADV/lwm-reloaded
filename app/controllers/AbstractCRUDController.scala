package controllers

import java.util.UUID

import controllers.helper._
import dao.AbstractDao
import dao.helper.TableFilter
import database._
import javax.inject.Inject
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
    with TableFilter[T]
    with JsonParser {

  import utils.Ops.unwrapTrys

  import scala.concurrent.ExecutionContext.Implicits.global

  protected implicit def writes: Writes[LwmModel]

  protected implicit def reads: Reads[Protocol]

  implicit def listReads[R](implicit r: Reads[R]): Reads[List[R]] = Reads.list[R]

  protected def abstractDao: AbstractDao[T, DbModel, LwmModel]

  protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    Failure(new Throwable(s"no filter for $attribute and $value"))
  }

  protected def toDbModel(protocol: Protocol, existingId: Option[UUID]): DbModel

  protected def toLwmModel(dbModels: TraversableOnce[DbModel]): Traversable[LwmModel] = dbModels.map(_.toUniqueEntity.asInstanceOf[LwmModel]).toTraversable

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
    val atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic

    (for {
      uuid <- id.uuidF
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
    id.uuidF.flatMap(delete0).deleted
  }

  protected def delete0(uuid: UUID): Future[LwmModel] = {
    abstractDao.delete(uuid).map(_.toUniqueEntity.asInstanceOf[LwmModel])
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

    (for {
      uuid <- id.uuidF
      model <- abstractDao.getSingle(uuid, atomic)
    } yield model).jsonResult(id)
  }

  protected implicit class AbstractTableFilter(string: String) {

    import dao.helper.TableFilter._

    def uuid: Try[UUID] = Try(UUID.fromString(string))

    def uuidF: Future[UUID] = Future.fromTry(string.uuid)

    def boolean: Try[Boolean] = Try(string.toBoolean)

    def int: Try[Int] = Try(string.toInt)

    def makeCourseFilter[A <: LabworkIdTable]: Try[A => Rep[Boolean]] = string.uuid map courseFilter

    def makeLabworkFilter[A <: LabworkIdTable]: Try[A => Rep[Boolean]] = string.uuid map labworkFilter

    def makeLabelLikeFilter[A <: LabelTable]: Try[A => Rep[Boolean]] = Success(labelFilterLike(string))

    def makeLabelEqualsFilter[A <: LabelTable]: Try[A => Rep[Boolean]] = Success(labelFilterEquals(string))

    def makeAbbrevFilter[A <: AbbreviationTable]: Try[A => Rep[Boolean]] = Success(abbreviationFilter(string))

    def makeUserFilter[A <: UserIdTable]: Try[A => Rep[Boolean]] = string.uuid map userFilter

    def makeRoomFilter[A <: RoomIdTable]: Try[A => Rep[Boolean]] = string.uuid map roomFilter

    def makeEntryTypeFilter[A <: EntryTypeTable]: Try[A => Rep[Boolean]] = Success(entryTypeFilter(string))

    def makeReportCardEntryFilter[A <: ReportCardEntryIdTable]: Try[A => Rep[Boolean]] = string.uuid map reportCardEntryFilter

    def makeUserByReportCardEntryFilter[A <: ReportCardEntryIdTable]: Try[A => Rep[Boolean]] = string.uuid map userByReportCardEntryFilter

    def makeLabworkByReportCardEntryFilter[A <: ReportCardEntryIdTable]: Try[A => Rep[Boolean]] = string.uuid map labworkByReportCardEntryFilter

    def makeCourseByReportCardEntryFilter[A <: ReportCardEntryIdTable]: Try[A => Rep[Boolean]] = string.uuid map courseByReportCardEntryFilter

    def makeRoomByReportCardEntryFilter[A <: ReportCardEntryIdTable]: Try[A => Rep[Boolean]] = string.uuid map roomByReportCardEntryFilter

    def makeGroupFilter[A <: GroupIdTable]: Try[A => Rep[Boolean]] = string.uuid map groupFilter
  }

}