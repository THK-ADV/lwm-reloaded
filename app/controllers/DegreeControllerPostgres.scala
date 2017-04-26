package controllers

import java.util.UUID

import models.{DegreeDb, DegreeProtocol, PostgresDegree}
import models.Permissions.{degree, god, prime}
import play.api.libs.json.{Reads, Writes}
import services._
import store.{DegreeTable, TableFilter}
import utils.LwmMimeType

import scala.util.{Failure, Try}

object DegreeControllerPostgres {
  lazy val labelAttribute = "label"
  lazy val abbreviationAttribute = "abbreviation"
}

final class DegreeControllerPostgres(val sessionService: SessionHandlingService, val roleService: RoleServiceLike, val degreeService: DegreeService)
  extends AbstractCRUDControllerPostgres[DegreeProtocol, DegreeTable, DegreeDb, PostgresDegree] {

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(degree.get)
    case GetAll => PartialSecureBlock(degree.getAll)
    case Update => PartialSecureBlock(prime)
    case _ => PartialSecureBlock(god)
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.degreeV1Json

  override protected implicit val writes: Writes[PostgresDegree] = PostgresDegree.writes

  override protected implicit val reads: Reads[DegreeProtocol] = PostgresDegree.reads

  override protected val abstractDao: AbstractDao[DegreeTable, DegreeDb, PostgresDegree] = degreeService

  override protected def tableFilter(attribute: String, values: Seq[String])(appendTo: Try[List[TableFilter[DegreeTable]]]): Try[List[TableFilter[DegreeTable]]] = {
    import controllers.DegreeControllerPostgres._

    (appendTo, (attribute, values)) match {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(DegreeLabelFilter(label.head)))
      case (list, (`abbreviationAttribute`, abbreviation)) => list.map(_.+:(DegreeAbbreviationFilter(abbreviation.head)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: DegreeProtocol, existingId: Option[UUID]): DegreeDb = DegreeDb.from(protocol, existingId)

  override protected def toLwmModel(dbModel: DegreeDb): PostgresDegree = dbModel.toDegree
}
