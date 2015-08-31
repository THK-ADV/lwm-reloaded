package models.security

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.{UniqueEntity, UriGenerator}
import play.api.libs.json.{Format, Json, Reads, Writes}

/*
 * BUG: Because the authority gets serialised together with a number of other sub-graphs (namely `RefRole`s),
 * two main subjects emerge that compete every time one tries to extract the main node
 * of an `Authority` graph. This in turn creates fluctuations in the test cases.
 * Mainly: `successfully delete an existing "authority" graph`
 *
 * Possible solution:
 * Do not save whole chunks of `RefRole` sub-graphs in an `Authority` but rather
 * just reference existing ones by their UUID.
 * This adds another step in the retrieval process (we might need to dereference the set
 * of UUID's) but avoids the unwanted deletion of existing `RefRole`s that get caught in
 * the deletion process of an `Authority`
 */

case class Authority(user: UUID, refRoles: Set[RefRole], id: UUID) extends UniqueEntity

case class AuthorityProtocol(user: UUID, refRoles: Set[RefRole])

case class RefRole(module: Option[UUID] = None, role: Role, id: UUID) extends UniqueEntity

case class RefRoleProtocol(module: Option[UUID] = None, role: Role)

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