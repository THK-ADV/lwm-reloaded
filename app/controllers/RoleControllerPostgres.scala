package controllers

import java.util.UUID

import models.Permissions.{god, prime, role}
import models._
import play.api.libs.json.{Reads, Writes}
import services._
import store.{RoleTable, TableFilter}
import utils.LwmMimeType

import scala.util.{Failure, Try}

object RoleControllerPostgres{
  lazy val labelAttribute = "label"
}

final class RoleControllerPostgres(val sessionService: SessionHandlingService, val abstractDao: RoleService2, val roleService: RoleServiceLike)
  extends AbstractCRUDControllerPostgres[PostgresRoleProtocol, RoleTable, RoleDb, Role]{
  override protected implicit val writes: Writes[Role] = Role.writes

  override protected implicit val reads: Reads[PostgresRoleProtocol] = PostgresRole.reads

  override protected def tableFilter(attribute: String, values: String)(appendTo: Try[List[TableFilter[RoleTable]]]): Try[List[TableFilter[RoleTable]]] = {
    import controllers.RoleControllerPostgres._

    (appendTo, (attribute, values)) match {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(RoleLabelFilter(label)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresRoleProtocol, existingId: Option[UUID]): RoleDb = RoleDb.from(protocol, existingId)

  override implicit val mimeType: LwmMimeType = LwmMimeType.roleV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(role.get)
    case GetAll => PartialSecureBlock(role.getAll)
    case Update => PartialSecureBlock(prime)
    case _ => PartialSecureBlock(god)
  }
}
