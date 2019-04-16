package controllers

import java.util.UUID

import dao._
import database.{LabworkApplicationDb, LabworkApplicationTable}
import javax.inject.{Inject, Singleton}
import models.Role.{EmployeeRole, StudentRole}
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain

import scala.util.{Failure, Try}

object LabworkApplicationController {
  lazy val labworkAttribute = "labwork"
  lazy val applicantAttribute = "applicant"
}

@Singleton
final class LabworkApplicationController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: LabworkApplicationDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[LabworkApplicationProtocol, LabworkApplicationTable, LabworkApplicationDb, LabworkApplicationLike](cc) {

  override protected implicit val writes: Writes[LabworkApplicationLike] = LabworkApplicationLike.writes

  override protected implicit val reads: Reads[LabworkApplicationProtocol] = LabworkApplicationProtocol.reads

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import LabworkApplicationController._

    (attribute, value) match {
      case (`labworkAttribute`, l) => l.makeLabworkFilter
      case (`applicantAttribute`, a) => a.makeUserFilter
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }
  }

  override protected def toDbModel(protocol: LabworkApplicationProtocol, existingId: Option[UUID]): LabworkApplicationDb = {
    LabworkApplicationDb(protocol.labwork, protocol.applicant, protocol.friends, id = existingId getOrElse UUID.randomUUID)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Create => PartialSecureBlock(List(StudentRole))
    case Update => PartialSecureBlock(List(StudentRole))
    case Delete => PartialSecureBlock(List(StudentRole))
    case Get => PartialSecureBlock(List(StudentRole))
    case GetAll => PartialSecureBlock(List(StudentRole, EmployeeRole))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbidden()
}
