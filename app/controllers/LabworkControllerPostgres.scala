package controllers

import java.util.UUID

import models.Permissions._
import models._
import play.api.libs.json.{Json, Reads, Writes}
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

final class LabworkControllerPostgres(val sessionService: SessionHandlingService, val roleService: RoleServiceLike, val labworkService: LabworkService) extends
  AbstractCRUDControllerPostgres[PostgresLabworkProtocol, LabworkTable, LabworkDb, LabworkProtocol] {

  override implicit def mimeType = LwmMimeType.labworkV1Json


  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(labwork.get)
    case GetAll => PartialSecureBlock(labwork.getAll)
    case _ => PartialSecureBlock(god)
  }

  override protected implicit def writes: Writes[PostgresLabwork] = PostgresLabwork.writes

  override protected implicit def reads: Reads[PostgresLabworkProtocol] = PostgresLabwork.reads

  override protected def abstractDao: AbstractDao[LabworkTable, LabworkDb, Labwork] = labworkService

  override protected def tableFilter(attribute: String, values: Seq[String])(appendTo: Try[List[TableFilter[LabworkTable]]]): Try[List[TableFilter[LabworkTable]]] = {
    import controllers.LabworkControllerPostgres._


    (appendTo, (attribute, values)) match {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(LabworkLabelFilter(label.head)))
      case (list, (`degreeAttribute`, label)) => list.map(_.+:(LabworkDegreeFilter(label.head)))
      case (list, (`semesterAttribute`, label)) => list.map(_.+:(LabworkSemesterFilter(label.head)))
      case (list, (`courseAttribute`, label)) => list.map(_.+:(LabworkCourseFilter(label.head)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresLabworkProtocol, existingId: Option[UUID]): LabworkDb = LabworkDb.from(protocol, existingId)

  override protected def toLwmModel(dbModel: LabworkDb): Labwork = dbModel.toLabwork
}