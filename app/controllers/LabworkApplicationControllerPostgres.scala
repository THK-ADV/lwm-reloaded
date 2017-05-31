package controllers

import java.util.UUID

import models.Permissions.labworkApplication
import models._
import play.api.libs.json.{Reads, Writes}
import services.{LabworkApplicationLabworkFilter, LabworkApplicationService2, RoleServiceLike, SessionHandlingService}
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

final class LabworkApplicationControllerPostgres(val sessionService: SessionHandlingService, val roleService: RoleServiceLike, val labworkApplicationService: LabworkApplicationService2)
  extends AbstractCRUDControllerPostgres[PostgresLabworkApplicationProtocol, LabworkApplicationTable, LabworkApplicationDb, LabworkApplication] {

  override protected implicit val writes: Writes[LabworkApplication] = LabworkApplication.writes

  override protected implicit val reads: Reads[PostgresLabworkApplicationProtocol] = PostgresLabworkApplication.reads

  override protected val abstractDao: LabworkApplicationService2 = labworkApplicationService

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[LabworkApplicationTable]]]): Try[List[TableFilter[LabworkApplicationTable]]] = {
    import controllers.LabworkApplicationControllerPostgres._

    (appendTo, (attribute, value)) match { // TODO expand by attributes
      case (list, (`labworkAttribute`, labworks)) => list.map(_.+:(LabworkApplicationLabworkFilter(labworks)))
    }
  }

  override protected def toDbModel(protocol: PostgresLabworkApplicationProtocol, existingId: Option[UUID]): LabworkApplicationDb = LabworkApplicationDb.from(protocol, existingId)

  override protected def toLwmModel(dbModel: LabworkApplicationDb): PostgresLabworkApplication = dbModel.toLabworkApplication

  override implicit val mimeType = LwmMimeType.labworkApplicationV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Create => PartialSecureBlock(labworkApplication.create)
    case Update => PartialSecureBlock(labworkApplication.update)
    case Delete => PartialSecureBlock(labworkApplication.delete)
    case Get => PartialSecureBlock(labworkApplication.get)
    case GetAll => PartialSecureBlock(labworkApplication.getAll)
  }
}
