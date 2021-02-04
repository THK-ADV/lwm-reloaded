package controllers

import dao.{AuthorityDao, ReportCardEntryTypeDao}
import database.{ReportCardEntryTypeDb, ReportCardEntryTypeTable}
import models.{ReportCardEntryType, ReportCardEntryTypeProtocol}
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.ControllerComponents
import security.LWMRole.{CourseAssistant, CourseEmployee, CourseManager, God}
import security.SecurityActionChain

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ReportCardEntryTypeController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val abstractDao: ReportCardEntryTypeDao,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
) extends AbstractCRUDController[ReportCardEntryTypeProtocol, ReportCardEntryTypeTable, ReportCardEntryTypeDb, ReportCardEntryType](cc) {

  import logger.AccessLoggingAction.log

  case class BatchUpdateRequest(users: List[UUID], assignmentEntry: UUID, entryType: String, bool: Option[Boolean], int: Option[Int])

  private def batchReads: Reads[BatchUpdateRequest] = Json.reads[BatchUpdateRequest]

  override protected implicit val writes: Writes[ReportCardEntryType] = ReportCardEntryType.writes

  override protected implicit val reads: Reads[ReportCardEntryTypeProtocol] = ReportCardEntryTypeProtocol.reads

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncAction log { request =>
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
    for {
      lid <- labwork.uuidF
      p <- Future.fromTry(parseJson(request)(batchReads))
      res <- if (p.bool.isDefined)
        abstractDao.updateFields(p.users, p.assignmentEntry, lid, p.entryType, p.bool.get)
      else if (p.int.isDefined)
        abstractDao.updateFields(p.users, p.assignmentEntry, lid, p.entryType, p.int.get)
      else
        Future.failed(new Throwable("either bool or int must be set"))
    } yield Created(Json.toJson(res))
  }

  override protected def toDbModel(protocol: ReportCardEntryTypeProtocol, existingId: Option[UUID]): ReportCardEntryTypeDb = ???

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Update => SecureBlock(restrictionId, List(CourseManager, CourseEmployee, CourseAssistant))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] =
    forbiddenAction()
}
