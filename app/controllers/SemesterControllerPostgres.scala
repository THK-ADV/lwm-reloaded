package controllers

import java.util.UUID

import dao._
import javax.inject.{Inject, Singleton}
import models.Role.{Admin, Employee, God, Student}
import models.{PostgresSemester, SemesterDb, SemesterProtocol}
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import store.{SemesterTable, TableFilter}
import utils.SecuredAction

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

@Singleton
final class SemesterControllerPostgres @Inject() (cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: SemesterDao, val secureAction: SecuredAction)
  extends AbstractCRUDControllerPostgres[SemesterProtocol, SemesterTable, SemesterDb, PostgresSemester](cc) {

  override protected implicit val writes: Writes[PostgresSemester] = PostgresSemester.writes

  override protected implicit val reads: Reads[SemesterProtocol] = SemesterProtocol.reads

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(Student, Employee))
    case GetAll => PartialSecureBlock(List(Student, Employee))
    case _ => PartialSecureBlock(List(Admin))
  }

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[SemesterTable]]]): Try[List[TableFilter[SemesterTable]]] = {
    import controllers.SemesterControllerPostgres._

    (appendTo, (attribute, value)) match {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(SemesterLabelFilter(label)))
      case (list, (`abbreviationAttribute`, abbrev)) => list.map(_.+:(SemesterAbbreviationFilter(abbrev)))
      case (list, (`startAttribute`, end)) => list.map(_.+:(SemesterStartFilter(end)))
      case (list, (`endAttribute`, start)) => list.map(_.+:(SemesterEndFilter(start)))
      case (list, (`sinceAttribute`, since)) => list.map(_.+:(SemesterSinceFilter(since)))
      case (list, (`untilAttribute`, until)) => list.map(_.+:(SemesterUntilFilter(until)))
      case (list, (`selectAttribute`, current)) if current == currentValue => list.map(_.+:(SemesterCurrentFilter()))
      case (_, (`selectAttribute`, other)) => Failure(new Throwable(s"Value of $selectAttribute should be $currentValue, but was $other"))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: SemesterProtocol, existingId: Option[UUID]): SemesterDb = SemesterDb.from(protocol, existingId)

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(List(God))
  }
}
