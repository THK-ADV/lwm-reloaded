package controllers

import java.util.UUID

import dao._
import javax.inject.{Inject, Singleton}
import models.Role.{EmployeeRole, StudentRole}
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import database.{LabworkApplicationDb, LabworkApplicationTable, TableFilter}
import security.SecurityActionChain

import scala.util.{Failure, Try}

object LabworkApplicationController {
  lazy val labworkAttribute = "labwork"
  lazy val applicantAttribute = "applicant"

  lazy val sinceAttribute = "since"
  lazy val untilAttribute = "until"
}

@Singleton
final class LabworkApplicationController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: LabworkApplicationDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[LabworkApplicationProtocol, LabworkApplicationTable, LabworkApplicationDb, LabworkApplicationLike](cc) {

  override protected implicit val writes: Writes[LabworkApplicationLike] = LabworkApplicationLike.writes

  override protected implicit val reads: Reads[LabworkApplicationProtocol] = LabworkApplicationProtocol.reads

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[LabworkApplicationTable]]]): Try[List[TableFilter[LabworkApplicationTable]]] = {
    import controllers.LabworkApplicationController._

    (appendTo, (attribute, value)) match {
      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(LabworkApplicationLabworkFilter(labwork)))
      case (list, (`applicantAttribute`, applicant)) => list.map(_.+:(LabworkApplicationApplicantFilter(applicant)))
      case (list, (`sinceAttribute`, since)) => Try(since.toLong).flatMap(l => list.map(_.+:(LabworkApplicationSinceFilter(l.toString))))
      case (list, (`untilAttribute`, until)) => Try(until.toLong).flatMap(l => list.map(_.+:(LabworkApplicationUntilFilter(l.toString))))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: LabworkApplicationProtocol, existingId: Option[UUID]): LabworkApplicationDb = LabworkApplicationDb.from(protocol, existingId)

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Create => PartialSecureBlock(List(StudentRole))
    case Update => PartialSecureBlock(List(StudentRole))
    case Delete => PartialSecureBlock(List(StudentRole))
    case Get => PartialSecureBlock(List(StudentRole))
    case GetAll => PartialSecureBlock(List(StudentRole, EmployeeRole))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbidden()
}
