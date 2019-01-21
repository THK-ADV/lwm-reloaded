package controllers

import java.util.UUID

import dao._
import javax.inject.{Inject, Singleton}
import models.Role.{Admin, Employee, God, Student}
import models.{DegreeDb, DegreeProtocol, PostgresDegree}
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import store.{DegreeTable, TableFilter}
import utils.SecuredAction

import scala.util.{Failure, Try}

object DegreeControllerPostgres {
  lazy val labelAttribute = "label"
  lazy val abbreviationAttribute = "abbreviation"
}

@Singleton
final class DegreeControllerPostgres @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: DegreeDao, val securedAction: SecuredAction)
  extends AbstractCRUDControllerPostgres[DegreeProtocol, DegreeTable, DegreeDb, PostgresDegree](cc) {

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(Employee, Student))
    case GetAll => PartialSecureBlock(List(Employee))
    case Update => PartialSecureBlock(List(Admin))
    case _ => PartialSecureBlock(List(God))
  }

  override protected implicit val writes: Writes[PostgresDegree] = PostgresDegree.writes

  override protected implicit val reads: Reads[DegreeProtocol] = DegreeProtocol.reads

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[DegreeTable]]]): Try[List[TableFilter[DegreeTable]]] = {
    import controllers.DegreeControllerPostgres._

    (appendTo, (attribute, value)) match {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(DegreeLabelFilter(label)))
      case (list, (`abbreviationAttribute`, abbreviation)) => list.map(_.+:(DegreeAbbreviationFilter(abbreviation)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: DegreeProtocol, existingId: Option[UUID]): DegreeDb = DegreeDb.from(protocol, existingId)

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbidden()
}
