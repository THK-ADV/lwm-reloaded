package controllers

import java.util.UUID

import dao.{AuthorityDao, RoleDao, RoleLabelFilter}
import javax.inject.{Inject, Singleton}
import models.Role.{God, RightsManager}
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import database.{RoleTable, TableFilter}
import utils.SecuredAction

import scala.util.{Failure, Try}

object RoleController {
  lazy val labelAttribute = "label"
}

@Singleton
final class RoleController @Inject()(cc: ControllerComponents, val abstractDao: RoleDao, val authorityDao: AuthorityDao, val securedAction: SecuredAction)
  extends AbstractCRUDController[PostgresRole, RoleTable, RoleDb, PostgresRole](cc) {

  override protected implicit val writes: Writes[PostgresRole] = PostgresRole.writes

  override protected implicit val reads: Reads[PostgresRole] = PostgresRole.reads

  override protected def tableFilter(attribute: String, values: String)(appendTo: Try[List[TableFilter[RoleTable]]]): Try[List[TableFilter[RoleTable]]] = {
    import controllers.RoleController._

    (appendTo, (attribute, values)) match {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(RoleLabelFilter(label)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresRole, existingId: Option[UUID]): RoleDb = ???

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(RightsManager))
    case GetAll => PartialSecureBlock(List(RightsManager))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbidden()
}
