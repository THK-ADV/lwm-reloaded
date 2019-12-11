package controllers

import java.util.UUID

import controllers.core.AbstractCRUDController
import dao._
import database.{DegreeDb, DegreeTable}
import javax.inject.{Inject, Singleton}
import models.Role.{Admin, EmployeeRole, God, StudentRole}
import models.{Degree, DegreeProtocol}
import org.joda.time.DateTime
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Try}

object DegreeController {
  lazy val labelAttribute = "label"
  lazy val abbreviationAttribute = "abbreviation"
}

@Singleton
final class DegreeController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val abstractDao: DegreeDao,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
) extends AbstractCRUDController[DegreeProtocol, DegreeTable, DegreeDb, Degree](cc) {

  import controllers.core.DBFilterOps._

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(EmployeeRole, StudentRole))
    case GetAll => PartialSecureBlock(List(EmployeeRole))
    case Update => PartialSecureBlock(List(Admin))
    case _ => PartialSecureBlock(List(God))
  }

  override protected implicit val writes: Writes[Degree] = Degree.writes

  override protected implicit val reads: Reads[DegreeProtocol] = DegreeProtocol.reads

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import DegreeController._

    (attribute, value) match {
      case (`labelAttribute`, l) => l.makeLabelEqualsFilter
      case (`abbreviationAttribute`, a) => a.makeAbbrevFilter
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }
  }

  override protected def toDbModel(protocol: DegreeProtocol, existingId: Option[UUID]): DegreeDb = {
    import utils.date.DateTimeOps.DateTimeConverter
    DegreeDb(protocol.label, protocol.abbreviation, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbiddenAction()
}
