package controllers

import java.util.UUID

import dao._
import database.{SemesterDb, SemesterTable}
import javax.inject.{Inject, Singleton}
import models.Role.{Admin, EmployeeRole, StudentRole}
import models.{Semester, SemesterProtocol}
import org.joda.time.DateTime
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain

import scala.util.{Failure, Success, Try}

object SemesterController {
  lazy val startAttribute = "start"
  lazy val endAttribute = "end"
  lazy val sinceAttribute = "since"
  lazy val untilAttribute = "until"
  lazy val labelAttribute = "label"
  lazy val abbreviationAttribute = "abbreviation"
  lazy val selectAttribute = "select"
  lazy val currentValue = "current"
}

@Singleton
final class SemesterController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: SemesterDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[SemesterProtocol, SemesterTable, SemesterDb, Semester](cc) {

  override protected implicit val writes: Writes[Semester] = Semester.writes

  override protected implicit val reads: Reads[SemesterProtocol] = SemesterProtocol.reads

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(StudentRole, EmployeeRole))
    case GetAll => PartialSecureBlock(List(StudentRole, EmployeeRole))
    case _ => PartialSecureBlock(List(Admin))
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import SemesterController._
    import dao.SemesterDao._
    import utils.date.DateTimeOps.StringConverter

    (attribute, value) match {
      case (`startAttribute`, s) => s.localDate map startFilter
      case (`endAttribute`, e) => e.localDate map endFilter
      case (`sinceAttribute`, s) => s.localDate map sinceFilter
      case (`untilAttribute`, u) => u.localDate map untilFilter
      case (`labelAttribute`, l) => l.makeLabelEqualsFilter
      case (`abbreviationAttribute`, a) => a.makeAbbrevFilter
      case (`selectAttribute`, `currentValue`) => Success(currentFilter())
      case (`selectAttribute`, other) => Failure(new Throwable(s"Value of $selectAttribute must be $currentValue, but was $other"))
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }
  }

  override protected def toDbModel(protocol: SemesterProtocol, existingId: Option[UUID]): SemesterDb = {
    import utils.date.DateTimeOps.{DateTimeConverter, LocalDateConverter}
    SemesterDb(protocol.label, protocol.abbreviation, protocol.start.sqlDate, protocol.end.sqlDate, protocol.examStart.sqlDate, DateTime.now.timestamp, None, existingId getOrElse UUID.randomUUID)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbidden()
}
