package controllers

import java.util.UUID

import dao.{AuthorityDao, ReportCardEntryTypeDao}
import database.{ReportCardEntryTypeDb, ReportCardEntryTypeTable}
import javax.inject.{Inject, Singleton}
import models.Role.{CourseAssistant, CourseEmployee, CourseManager, God}
import models.{ReportCardEntryType, ReportCardEntryTypeProtocol}
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}

@Singleton
class ReportCardEntryTypeController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val abstractDao: ReportCardEntryTypeDao,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
) extends AbstractCRUDController[ReportCardEntryTypeProtocol, ReportCardEntryTypeTable, ReportCardEntryTypeDb, ReportCardEntryType](cc) {

  override protected implicit val writes: Writes[ReportCardEntryType] = ReportCardEntryType.writes

  override protected implicit val reads: Reads[ReportCardEntryTypeProtocol] = ReportCardEntryTypeProtocol.reads

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncAction { request =>
    (for {
      uuid <- id.uuidF
      protocol <- Future.fromTry(parseJson(request))
      _ <- abstractDao.updateFields(uuid, protocol.bool, protocol.int)
    } yield ReportCardEntryType(
      protocol.entryType,
      protocol.bool,
      protocol.int,
      uuid
    )).jsonResult
  }

  def batchUpdate(course: String, labwork: String) = restrictedContext(course)(Update) asyncAction { request =>
    ???
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Update => SecureBlock(restrictionId, List(CourseManager, CourseEmployee, CourseAssistant))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] =
    Failure(new Throwable("no filter attributes allowed"))

  override protected def toDbModel(protocol: ReportCardEntryTypeProtocol, existingId: Option[UUID]): ReportCardEntryTypeDb = ???

  override protected def contextFrom: PartialFunction[Rule, SecureContext] =
    forbiddenAction()
}
