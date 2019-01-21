package controllers

import java.util.UUID

import dao._
import javax.inject.{Inject, Singleton}
import models.Role.{Employee, Student}
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import store.{LabworkApplicationTable, TableFilter}
import utils.SecuredAction

import scala.util.{Failure, Try}

object LabworkApplicationControllerPostgres {
  lazy val labworkAttribute = "labwork"
  lazy val applicantAttribute = "applicant"

  lazy val sinceAttribute = "since"
  lazy val untilAttribute = "until"
}

@Singleton
final class LabworkApplicationControllerPostgres @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: LabworkApplicationDao, val securedAction: SecuredAction)
  extends AbstractCRUDControllerPostgres[PostgresLabworkApplicationProtocol, LabworkApplicationTable, LabworkApplicationDb, LabworkApplication](cc) {

  override protected implicit val writes: Writes[LabworkApplication] = LabworkApplication.writes

  override protected implicit val reads: Reads[PostgresLabworkApplicationProtocol] = PostgresLabworkApplicationProtocol.reads

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[LabworkApplicationTable]]]): Try[List[TableFilter[LabworkApplicationTable]]] = {
    import controllers.LabworkApplicationControllerPostgres._

    (appendTo, (attribute, value)) match {
      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(LabworkApplicationLabworkFilter(labwork)))
      case (list, (`applicantAttribute`, applicant)) => list.map(_.+:(LabworkApplicationApplicantFilter(applicant)))
      case (list, (`sinceAttribute`, since)) => Try(since.toLong).flatMap(l => list.map(_.+:(LabworkApplicationSinceFilter(l.toString))))
      case (list, (`untilAttribute`, until)) => Try(until.toLong).flatMap(l => list.map(_.+:(LabworkApplicationUntilFilter(l.toString))))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresLabworkApplicationProtocol, existingId: Option[UUID]): LabworkApplicationDb = LabworkApplicationDb.from(protocol, existingId)

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Create => PartialSecureBlock(List(Student))
    case Update => PartialSecureBlock(List(Student))
    case Delete => PartialSecureBlock(List(Student))
    case Get => PartialSecureBlock(List(Student))
    case GetAll => PartialSecureBlock(List(Student, Employee))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbidden()
}
