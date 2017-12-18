package controllers

import java.util.UUID

import dao._
import utils.LwmDateTime._
import models.Permissions.{prime, semester}
import models.Role.{Admin, Employee, Student}
import models.{PostgresSemester, SemesterDb, SemesterProtocol}
import org.joda.time.LocalDate
import play.api.libs.json.{Reads, Writes}
import services._
import store.{SemesterTable, TableFilter}
import utils.LwmMimeType

import scala.util.{Failure, Try}

object SemesterControllerPostgres {
  lazy val startAttribute = "start"
  lazy val endAttribute = "end"
  lazy val sinceAttribute = "since"
  lazy val untilAttribute = "until"
  lazy val labelAttribute = "label"
  lazy val abbreviationAttribute = "abbreviation"
  lazy val selectAttribute = "select"
  lazy val currentValue = "current"
}

final class SemesterControllerPostgres(val sessionService: SessionHandlingService, val authorityDao: AuthorityDao, val abstractDao: SemesterDao)
  extends AbstractCRUDControllerPostgres[SemesterProtocol, SemesterTable, SemesterDb, PostgresSemester] {

  override implicit val mimeType = LwmMimeType.semesterV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(Student, Employee))
    case GetAll => PartialSecureBlock(List(Student, Employee))
    case _ => PartialSecureBlock(List(Admin))
  }

  override protected implicit val writes: Writes[PostgresSemester] = PostgresSemester.writes

  override protected implicit val reads: Reads[SemesterProtocol] = PostgresSemester.reads

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[SemesterTable]]]): Try[List[TableFilter[SemesterTable]]] = {
    import controllers.SemesterControllerPostgres._

    (appendTo, (attribute, value)) match {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(SemesterLabelFilter(label)))
      case (list, (`abbreviationAttribute`, abbrev)) => list.map(_.+:(SemesterAbbreviationFilter(abbrev)))
      case (list, (`startAttribute`, end)) => list.map(_.+:(SemesterStartFilter(end)))
      case (list, (`endAttribute`, start)) => list.map(_.+:(SemesterEndFilter(start)))
      case (list, (`sinceAttribute`, since)) => list.map(_.+:(SemesterSinceFilter(since)))
      case (list, (`untilAttribute`, until)) => list.map(_.+:(SemesterUntilFilter(until)))
      case (list, (`selectAttribute`, current)) if current == currentValue => list.map(_.+:(SemesterCurrentFilter(LocalDate.now.stringMillis)))
      case (_, (`selectAttribute`, other)) => Failure(new Throwable(s"Value of $selectAttribute should be $currentValue, but was $other"))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: SemesterProtocol, existingId: Option[UUID]): SemesterDb = SemesterDb.from(protocol, existingId)
}