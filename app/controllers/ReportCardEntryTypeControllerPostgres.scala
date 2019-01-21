package controllers

import java.util.UUID

import dao.{AuthorityDao, ReportCardEntryTypeDao}
import javax.inject.{Inject, Singleton}
import models.Role.{CourseAssistant, CourseEmployee, CourseManager, God}
import models.{PostgresReportCardEntryType, PostgresReportCardEntryTypeProtocol, ReportCardEntryTypeDb}
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import store.{ReportCardEntryTypeTable, TableFilter}
import utils.SecuredAction

import scala.concurrent.Future
import scala.util.{Success, Try}

@Singleton
class ReportCardEntryTypeControllerPostgres @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: ReportCardEntryTypeDao, val securedAction: SecuredAction)
  extends AbstractCRUDControllerPostgres[PostgresReportCardEntryTypeProtocol, ReportCardEntryTypeTable, ReportCardEntryTypeDb, PostgresReportCardEntryType](cc) {

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[PostgresReportCardEntryType] = PostgresReportCardEntryType.writes

  override protected implicit val reads: Reads[PostgresReportCardEntryTypeProtocol] = PostgresReportCardEntryTypeProtocol.reads

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncAction { request =>
    val uuid = UUID.fromString(id)

    (for {
      protocol <- Future.fromTry(parseJson(request))
      updated <- abstractDao.updateFields(uuid, protocol.bool, protocol.int)
    } yield if (updated) Some(protocol) else None).jsonResult(uuid)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Update => SecureBlock(restrictionId, List(CourseManager, CourseEmployee, CourseAssistant))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[ReportCardEntryTypeTable]]]): Try[List[TableFilter[ReportCardEntryTypeTable]]] = Success(List.empty)

  override protected def toDbModel(protocol: PostgresReportCardEntryTypeProtocol, existingId: Option[UUID]): ReportCardEntryTypeDb = ???

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbidden()
}
