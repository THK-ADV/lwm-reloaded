package controllers

import java.util.UUID

import dao.{AuthorityDao, RoleDao, RoleLabelFilter}
import models.Role.{God, RightsManager}
import models._
import play.api.libs.json.{Reads, Writes}
import services._
import store.{RoleTable, TableFilter}
import utils.LwmMimeType

import scala.util.{Failure, Try}

object RoleControllerPostgres {
  lazy val labelAttribute = "label"
}

final class RoleControllerPostgres(val sessionService: SessionHandlingService, val abstractDao: RoleDao, val authorityDao: AuthorityDao)
  extends AbstractCRUDControllerPostgres[PostgresRole, RoleTable, RoleDb, PostgresRole] {

  override protected implicit val writes: Writes[PostgresRole] = PostgresRole.writes

  override protected implicit val reads: Reads[PostgresRole] = PostgresRole.reads

  override protected def tableFilter(attribute: String, values: String)(appendTo: Try[List[TableFilter[RoleTable]]]): Try[List[TableFilter[RoleTable]]] = {
    import controllers.RoleControllerPostgres._

    (appendTo, (attribute, values)) match {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(RoleLabelFilter(label)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresRole, existingId: Option[UUID]): RoleDb = ???

  override implicit val mimeType: LwmMimeType = LwmMimeType.roleV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(RightsManager))
    case GetAll => PartialSecureBlock(List(RightsManager))
    case _ => PartialSecureBlock(List(God))
  }
}
