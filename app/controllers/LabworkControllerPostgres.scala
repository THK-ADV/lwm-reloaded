package controllers

import java.util.UUID

import dao._
import models.Permissions._
import models._
import play.api.libs.json.{Reads, Writes}
import services._
import store.{LabworkTable, TableFilter}
import utils.LwmMimeType

import scala.util.{Failure, Try}

object LabworkControllerPostgres {
  lazy val labelAttribute = "label"
  lazy val degreeAttribute = "degree"
  lazy val semesterAttribute = "semester"
  lazy val courseAttribute = "course"
}

final class LabworkControllerPostgres(val sessionService: SessionHandlingService, val authorityDao: AuthorityDao, val abstractDao: LabworkDao) extends
  AbstractCRUDControllerPostgres[PostgresLabworkProtocol, LabworkTable, LabworkDb, Labwork] {

  override implicit def mimeType = LwmMimeType.labworkV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(labwork.get)
    case GetAll => PartialSecureBlock(labwork.getAll)
    case _ => PartialSecureBlock(god)
  }

  override protected implicit def writes: Writes[Labwork] = Labwork.writes

  override protected implicit def reads: Reads[PostgresLabworkProtocol] = Labwork.reads

  override protected def tableFilter(attribute: String, values: String)(appendTo: Try[List[TableFilter[LabworkTable]]]): Try[List[TableFilter[LabworkTable]]] = {
    import controllers.LabworkControllerPostgres._

    (appendTo, (attribute, values)) match {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(LabworkLabelFilter(label)))
      case (list, (`degreeAttribute`, degree)) => list.map(_.+:(LabworkDegreeFilter(degree)))
      case (list, (`semesterAttribute`, semester)) => list.map(_.+:(LabworkSemesterFilter(semester)))
      case (list, (`courseAttribute`, course)) => list.map(_.+:(LabworkCourseFilter(course)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresLabworkProtocol, existingId: Option[UUID]): LabworkDb = LabworkDb.from(protocol, existingId)
}