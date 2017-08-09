package controllers

import java.util.UUID

import dao.{AuthorityDao, LabworkApplicationDao, LabworkApplicationLabworkFilter}
import models.Permissions.labworkApplication
import models.Role.{Employee, Student}
import models._
import play.api.libs.json.{Reads, Writes}
import services.SessionHandlingService
import store.{LabworkApplicationTable, TableFilter}
import utils.LwmMimeType

import scala.util.Try

object LabworkApplicationControllerPostgres {
  lazy val labworkAttribute = "labwork"
  lazy val applicantAttribute = "applicant"
  lazy val friendAttribute = "friend"
  lazy val dateAttribute = "date"
  lazy val minTimeAttribute = "minTime"
  lazy val maxTimeAttribute = "maxTime"
}

final class LabworkApplicationControllerPostgres(val sessionService: SessionHandlingService, val authorityDao: AuthorityDao, val abstractDao: LabworkApplicationDao)
  extends AbstractCRUDControllerPostgres[PostgresLabworkApplicationProtocol, LabworkApplicationTable, LabworkApplicationDb, LabworkApplication] {

  override protected implicit val writes: Writes[LabworkApplication] = LabworkApplication.writes

  override protected implicit val reads: Reads[PostgresLabworkApplicationProtocol] = PostgresLabworkApplication.reads

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[LabworkApplicationTable]]]): Try[List[TableFilter[LabworkApplicationTable]]] = {
    import controllers.LabworkApplicationControllerPostgres._

    (appendTo, (attribute, value)) match { // TODO expand attributes
      case (list, (`labworkAttribute`, labworks)) => list.map(_.+:(LabworkApplicationLabworkFilter(labworks)))
    }
  }

  override protected def toDbModel(protocol: PostgresLabworkApplicationProtocol, existingId: Option[UUID]): LabworkApplicationDb = LabworkApplicationDb.from(protocol, existingId)

  override implicit val mimeType = LwmMimeType.labworkApplicationV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Create => PartialSecureBlock(List(Student))
    case Update => PartialSecureBlock(List(Student))
    case Delete => PartialSecureBlock(List(Student))
    case Get => PartialSecureBlock(List(Student))
    case GetAll => PartialSecureBlock(List(Student, Employee))
  }
}
