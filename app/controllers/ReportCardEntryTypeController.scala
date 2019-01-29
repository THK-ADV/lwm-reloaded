package controllers

import java.util.UUID

import dao.{AuthorityDao, ReportCardEntryTypeDao}
import javax.inject.{Inject, Singleton}
import models.Role.{CourseAssistant, CourseEmployee, CourseManager, God}
import models.{ReportCardEntryType, ReportCardEntryTypeProtocol}
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import database.{ReportCardEntryTypeDb, ReportCardEntryTypeTable, TableFilter}
import security.SecurityActionChain

import scala.concurrent.Future
import scala.util.{Success, Try}

@Singleton
class ReportCardEntryTypeController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: ReportCardEntryTypeDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[ReportCardEntryTypeProtocol, ReportCardEntryTypeTable, ReportCardEntryTypeDb, ReportCardEntryType](cc) {

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[ReportCardEntryType] = ReportCardEntryType.writes

  override protected implicit val reads: Reads[ReportCardEntryTypeProtocol] = ReportCardEntryTypeProtocol.reads

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

  override protected def toDbModel(protocol: ReportCardEntryTypeProtocol, existingId: Option[UUID]): ReportCardEntryTypeDb = ???

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbidden()
}
