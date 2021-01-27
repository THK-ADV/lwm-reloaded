package controllers

import controllers.helper._
import dao.AbstractDao
import dao.helper.TableFilter
import database._
import models.{UniqueDbEntity, UniqueEntity}
import play.api.libs.json._
import play.api.mvc._
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Rep

import java.util.UUID
import javax.inject.Inject
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

  import logger.AccessLoggingAction.log

  import scala.concurrent.ExecutionContext.Implicits.global

  protected implicit def writes: Writes[LwmModel]

  protected implicit def reads: Reads[Protocol]

  protected def abstractDao: AbstractDao[T, DbModel, LwmModel]

  protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    Failure(new Throwable(s"no filter for $attribute and $value"))
  }

  protected def toDbModel(protocol: Protocol, existingId: Option[UUID]): DbModel

  protected def toLwmModel(dbModel: DbModel): LwmModel = dbModel.toUniqueEntity.asInstanceOf[LwmModel]

  protected def parsed(id: Option[UUID], action: DbModel => Future[DbModel])(implicit request: Request[AnyContent]): Future[LwmModel] = {
    for {
      protocol <- Future.fromTry(parseJson(request)(reads))
      dbModel = toDbModel(protocol, id)
      result <- action(dbModel)
      atomic = isAtomic(default = false)
      lwmModel <- if (atomic)
        abstractDao.getSingle(result.id, atomic)
      else
        Future.successful(Some(toLwmModel(result)))
      if lwmModel.isDefined
    } yield lwmModel.get
  }

  def create(secureContext: SecureContext = contextFrom(Create)) = secureContext asyncAction log { implicit request =>
    parsed(None, abstractDao.create)
      .created
  }

  def update(id: String, secureContext: SecureContext = contextFrom(Update)) = secureContext asyncAction log { implicit request =>
    id.uuidF
      .flatMap(uuid => parsed(Some(uuid), abstractDao.update))
      .jsonResult
  }

  def invalidate(id: String, secureContext: SecureContext = contextFrom(Delete)) = secureContext asyncAction log { _ =>
    id.uuidF.flatMap(invalidate0).jsonResult
  }

  protected def invalidate0(uuid: UUID): Future[LwmModel] = {
    abstractDao.invalidate(uuid).map(toLwmModel)
  }

  def all(secureContext: SecureContext = contextFrom(GetAll)) = secureContext asyncAction { implicit request =>
    allWithFilter { (filter, defaults) =>
      abstractDao.get(filter, defaults.atomic, defaults.valid, defaults.lastModified).jsonResult
    }
  }

  // TODO use this to count number of entries
  def count(secureContext: SecureContext = contextFrom(GetAll)) = secureContext asyncAction { implicit request =>
    allWithFilter { (filter, defaults) =>
      abstractDao.count(filter, defaults.valid, defaults.lastModified).jsonResult
    }
  }

  protected def allWithFilter[A](f: (List[TableFilterPredicate], DefaultAttributes) => Future[Result])(implicit request: Request[AnyContent]): Future[Result] = {
    import utils.Ops.MonadInstances.tryM
    import utils.Ops._

    val (queryString, defaults) = extractAttributes(request.queryString)
    val filter = makeTableFilter(queryString).sequence

    for {
      filter <- Future.fromTry(filter)
      result <- f(filter, defaults)
    } yield result
  }

  private def makeTableFilter(queryString: QueryString): List[Try[TableFilterPredicate]] = {
    (for {
      (attribute, values) <- queryString if values.nonEmpty
    } yield makeTableFilter(attribute, values.head)).toList
  }

  def get(id: String, secureContext: SecureContext = contextFrom(Get)) = secureContext asyncAction { implicit request =>
    val atomic = isAtomic(default = true)

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