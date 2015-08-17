package models.security

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.{UniqueEntity, UriGenerator}
import play.api.libs.json.{Format, Json, Reads, Writes}

sealed trait ContextualRole

case class Authority(user: UUID, refRoles: Set[RefRole], id: UUID) extends UniqueEntity

case class AuthorityProtocol(user: UUID, authority: Set[RefRole])

case class RefRole(module: Option[UUID] = None, role: Role, id: UUID) extends UniqueEntity

case class RefRoleProtocol(role: Role, module: Option[UUID] = None)

case class Role(name: String, permissions: Set[Permission])

case class Permission(value: String) {
  override def toString: String = value
}

object Permissions {

  val createCourse = Permission("create a new course")

  val joinLabwork = Permission("join an existing labwork")
}

object Authority extends UriGenerator[Authority] with JsonSerialisation[AuthorityProtocol, Authority] {
  def empty = Authority(UUID.randomUUID(), Set.empty[RefRole], UUID.randomUUID())

  override def base: String = "authorities"

  override implicit def reads: Reads[AuthorityProtocol] = Json.reads[AuthorityProtocol]

  override implicit def writes: Writes[Authority] = Json.writes[Authority]
}

object Roles {

  import Permissions._

  val admin = Role("admin", Set(createCourse))

  val user = Role("user", Set(joinLabwork))
}

object Permission extends JsonSerialisation[Permission, Permission] {

  override implicit def reads: Reads[Permission] = Json.reads[Permission]

  override implicit def writes: Writes[Permission] = Json.writes[Permission]
}

object Role extends JsonSerialisation[Role, Role] {

  override implicit def reads: Reads[Role] = Json.reads[Role]

  override implicit def writes: Writes[Role] = Json.writes[Role]
}

object RefRole extends UriGenerator[RefRole] with JsonSerialisation[RefRoleProtocol, RefRole] {
  override def base: String = "refRoles"

  implicit def format: Format[RefRole] = Json.format[RefRole]

  override implicit def reads: Reads[RefRoleProtocol] = Json.reads[RefRoleProtocol]

  override implicit def writes: Writes[RefRole] = Json.writes[RefRole]
}