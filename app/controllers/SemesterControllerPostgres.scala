package controllers

import java.util.UUID

import models.Permissions.{prime, semester}
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
  lazy val labelAttribute = "label"
  lazy val abbreviationAttribute = "abbreviation"
  lazy val selectAttribute = "select"
  lazy val currentValue = "current"
}

final class SemesterControllerPostgres(val sessionService: SessionHandlingService, val roleService: RoleServiceLike, val semesterService: SemesterService)
  extends AbstractCRUDControllerPostgres[SemesterProtocol, SemesterTable, SemesterDb, PostgresSemester] {

  override implicit val mimeType = LwmMimeType.semesterV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(semester.get)
    case GetAll => PartialSecureBlock(semester.getAll)
    case _ => PartialSecureBlock(prime)
  }

  override protected implicit val writes: Writes[PostgresSemester] = PostgresSemester.writes

  override protected implicit val reads: Reads[SemesterProtocol] = PostgresSemester.reads

  override protected val abstractDao: AbstractDao[SemesterTable, SemesterDb, PostgresSemester] = semesterService

  override protected def idTableFilter(id: String): TableFilter[SemesterTable] = SemesterIdFilter(id)

  override protected def tableFilter(attribute: String, values: Seq[String])(appendTo: Try[List[TableFilter[SemesterTable]]]): Try[List[TableFilter[SemesterTable]]] = {
    import controllers.SemesterControllerPostgres._

    (appendTo, (attribute, values)) match {
      case (list, (`labelAttribute`, labels)) => list.map(_.+:(SemesterLabelFilter(labels.head)))
      case (list, (`abbreviationAttribute`, abbrevs)) => list.map(_.+:(SemesterAbbreviationFilter(abbrevs.head)))
      case (list, (`startAttribute`, starts)) => list.map(_.+:(SemesterStartFilter(starts.head)))
      case (list, (`endAttribute`, ends)) => list.map(_.+:(SemesterEndFilter(ends.head)))
      case (list, (`selectAttribute`, current)) if current.head == currentValue => list.map(_.+:(SemesterCurrentFilter(LocalDate.now.toString)))
      case (_, (`selectAttribute`, other)) => Failure(new Throwable(s"Value of $selectAttribute should be $currentValue, but was ${other.head}"))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: SemesterProtocol, existingId: Option[UUID]): SemesterDb = SemesterDb.from(protocol, existingId)

  override protected def toLwmModel(dbModel: SemesterDb): PostgresSemester = dbModel.toSemester
}
