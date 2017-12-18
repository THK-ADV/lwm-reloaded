package controllers

import java.util.UUID

import dao.{AuthorityDao, ReportCardEntryTypeDao}
import models.Role.{CourseAssistant, CourseEmployee, CourseManager, God}
import models.{PostgresReportCardEntryType, PostgresReportCardEntryTypeProtocol, ReportCardEntryTypeDb}
import play.api.libs.json.{Reads, Writes}
import services.SessionHandlingService
import store.{ReportCardEntryTypeTable, TableFilter}
import utils.LwmMimeType

import scala.concurrent.Future
import scala.util.Try

class ReportCardEntryTypeControllerPostgres(val authorityDao: AuthorityDao,
                                            val sessionService: SessionHandlingService,
                                            val abstractDao: ReportCardEntryTypeDao
                                           ) extends AbstractCRUDControllerPostgres[PostgresReportCardEntryTypeProtocol, ReportCardEntryTypeTable, ReportCardEntryTypeDb, PostgresReportCardEntryType] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[PostgresReportCardEntryType] = PostgresReportCardEntryType.writes

  override protected implicit val reads: Reads[PostgresReportCardEntryTypeProtocol] = PostgresReportCardEntryType.reads

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    val uuid = UUID.fromString(id)

    (for {
      protocol <- Future.fromTry(parse[PostgresReportCardEntryTypeProtocol](request))
      updated <- abstractDao.updateFields(uuid, protocol.bool, protocol.int)
    } yield if (updated) Some(protocol) else None).jsonResult(uuid)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Update => SecureBlock(restrictionId, List(CourseManager, CourseEmployee, CourseAssistant))
    case _ => PartialSecureBlock(List(God))
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardEntryTypeV1Json

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[ReportCardEntryTypeTable]]]): Try[List[TableFilter[ReportCardEntryTypeTable]]] = ???

  override protected def toDbModel(protocol: PostgresReportCardEntryTypeProtocol, existingId: Option[UUID]): ReportCardEntryTypeDb = ???

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(List(God))
  }
}
