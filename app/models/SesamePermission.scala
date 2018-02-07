package models

import java.sql.Timestamp
import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.json.{Json, Writes}
import utils.LwmDateTime.DateTimeConverter

/**
  * A unary permission.
  *
  * @param value Raw permission label
  */

case class SesamePermission(value: String) {
  override def toString: String = value
}

case class PostgresPermission(value: String, description: String, id: UUID = UUID.randomUUID) extends UniqueEntity

case class PermissionDb(value: String, description: String, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresPermission(value, description, id)
}

object PostgresPermission {
  implicit val writes: Writes[PostgresPermission] = Json.writes[PostgresPermission]
}