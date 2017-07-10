package controllers

import java.util.UUID

import models.{PermissionDb, PostgresPermission, PostgresPermissionProtocol}
import play.api.libs.json.{Reads, Writes}
import services._
import store.{PermissionTable, TableFilter}
import utils.LwmMimeType

import scala.util.{Failure, Try}

/**
  * Created by florian on 5/29/17.
  */
object PermissionControllerPostgres {
  lazy val prefixAttribute = "prefix"
  lazy val suffixAttribute = "suffix"
  lazy val descriptionAttribute = "description"
  lazy val valueAttribute = "value"
}

final class PermissionControllerPostgres(val roleService: RoleService, val sessionService: SessionHandlingService, val permissionService: PermissionService)
  extends AbstractCRUDControllerPostgres[PostgresPermissionProtocol, PermissionTable, PermissionDb, PostgresPermission]{

  override protected implicit val writes: Writes[PostgresPermission] = PostgresPermission.writes

  override protected implicit val reads: Reads[PostgresPermissionProtocol] = PostgresPermission.reads

  override protected val abstractDao: AbstractDao[PermissionTable, PermissionDb, PostgresPermission] = permissionService

  override protected def tableFilter(attribute: String, values: Seq[String])(appendTo: Try[List[TableFilter[PermissionTable]]]): Try[List[TableFilter[PermissionTable]]] = {
    import controllers.PermissionControllerPostgres._

    (appendTo, (attribute, values)) match {
      case (list, (`prefixAttribute`, prefix)) => list.map(_.+:(PermissionPrefixFilter(prefix.head)))
      case (list, (`suffixAttribute`, suffix)) => list.map(_.+:(PermissionSuffixFilter(suffix.head)))
      case (list, (`valueAttribute`, value)) => list.map(_.+:(PermissionValueFilter(value.head)))
      case (list, (`descriptionAttribute`, description)) => list.map(_.+:(PermissionDescriptionFilter(description.head)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresPermissionProtocol, existingId: Option[UUID]): PermissionDb = PermissionDb.from(protocol, existingId)

  override protected def toLwmModel(dbModel: PermissionDb): PostgresPermission = dbModel.toPermission

  override implicit def mimeType: LwmMimeType = LwmMimeType.permissionV1Json
}