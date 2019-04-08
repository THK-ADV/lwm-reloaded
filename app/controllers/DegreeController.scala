package controllers

import java.util.UUID

import dao._
import database.{DegreeDb, DegreeTable}
import javax.inject.{Inject, Singleton}
import models.Role.{Admin, EmployeeRole, God, StudentRole}
import models.{Degree, DegreeProtocol}
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain

//object DegreeController {
//  lazy val labelAttribute = "label"
//  lazy val abbreviationAttribute = "abbreviation"
//}

@Singleton
final class DegreeController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: DegreeDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[DegreeProtocol, DegreeTable, DegreeDb, Degree](cc) {

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(EmployeeRole, StudentRole))
    case GetAll => PartialSecureBlock(List(EmployeeRole))
    case Update => PartialSecureBlock(List(Admin))
    case _ => PartialSecureBlock(List(God))
  }

  override protected implicit val writes: Writes[Degree] = Degree.writes

  override protected implicit val reads: Reads[DegreeProtocol] = DegreeProtocol.reads

//  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[DegreeTable]]]): Try[List[TableFilter[DegreeTable]]] = {
//    import controllers.DegreeController._
//
//    (appendTo, (attribute, value)) match {
//      case (list, (`labelAttribute`, label)) => list.map(_.+:(DegreeLabelFilter(label)))
//      case (list, (`abbreviationAttribute`, abbreviation)) => list.map(_.+:(DegreeAbbreviationFilter(abbreviation)))
//      case _ => Failure(new Throwable("Unknown attribute"))
//    }
//  }

  override protected def toDbModel(protocol: DegreeProtocol, existingId: Option[UUID]): DegreeDb = DegreeDb.from(protocol, existingId)

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbidden()
}
