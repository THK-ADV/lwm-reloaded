package models.security

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.users.{Employee, Student, User}
import models.{Course, UniqueEntity, UriGenerator}
import play.api.libs.json._

/**
 * Structure linking a user to his/her respective authority in the system.
 * `Authority` is created in order to separate concerns between user data and
 * his/her permissions in the underlying system.
 * It abstracts over the set of all partial permissions a user has in the system.
 *
 * @param user The referenced user
 * @param refRoles User roles relative to a module
 * @param id Unique id of the `Authority`
 */


case class Authority(user: UUID, refRoles: Set[UUID], id: UUID = Authority.randomUUID) extends UniqueEntity

case class AuthorityProtocol(user: UUID, refRoles: Set[UUID])

case class AuthorityAtom(user: User, refRoles: Set[RefRoleAtom], id: UUID)

/**
 * Structure binding a particular module to a particular `Role`(or set of permissions).
 * `RefRole`s bind `Role`s and modules together.
 * This in turn grants users specific permissions in specific cases.
 *  i.e:
 *    AP1 -> Coworker
 *    BS -> Student
 *
 * Because `Role`s need to be independent, they are only referenced in this graph.
 * Directly integrating them in the `RefRole` graph would mean that, upon deletion,
 * the `Role`s themselves would also be deleted.
 *
 * @param course Referenced course/module
 * @param role Reference to `Role` Instance of that course/module
 * @param id Unique id of the `RefRole`
 */
case class RefRole(course: Option[UUID] = None, role: UUID, id: UUID = RefRole.randomUUID) extends UniqueEntity

case class RefRoleProtocol(course: Option[UUID] = None, role: UUID)

case class RefRoleAtom(course: Option[Course], role: Role, id: UUID)

/**
 * Structure abstracting over a set of unary `Permission`s.
 * These sets are aggregated to specific `Role`s such that default, reusable `Role`s are possible.
 * `Role`s are independent. They can only be referenced by other graphs.
 *
 * @param label Name or label of the `Role`
 * @param permissions The unary permissions of that `Role`
 */

case class Role(label: String, permissions: Set[Permission], id: UUID = Role.randomUUID) extends UniqueEntity

case class RoleProtocol(name: String, permissions: Set[Permission])

object Roles {
  lazy val Admin = "Administrator"
  lazy val Employee = "Mitarbeiter"
  lazy val Student = "Student"
  lazy val CourseEmployee = "Modulmitarbeiter"
  lazy val CourseAssistant = "Hilfskraft"
  lazy val CourseManager = "Modulverantwortlicher"
  lazy val RightsManager = "Rechteverantwortlicher"
}

/**
 * A unary permission.
 *
 * @param value Raw permission label
 */

case class Permission(value: String) {
  override def toString: String = value
}

object Permission extends JsonSerialisation[Permission, Permission] {

  override implicit def reads: Reads[Permission] = Json.reads[Permission]

  override implicit def writes: Writes[Permission] = Json.writes[Permission]
}

object Role extends UriGenerator[Role] with JsonSerialisation[RoleProtocol, Role] {

  implicit def format: Format[Role] = Json.format[Role]

  override implicit def reads: Reads[RoleProtocol] = Json.reads[RoleProtocol]

  override implicit def writes: Writes[Role] = Json.writes[Role]

  override def base: String = "roles"
}

object RefRole extends UriGenerator[RefRole] with JsonSerialisation[RefRoleProtocol, RefRole] {
  import Role._

  override def base: String = "refRoles"

  implicit def format: Format[RefRole] = Json.format[RefRole]

  override implicit def reads: Reads[RefRoleProtocol] = Json.reads[RefRoleProtocol]

  override implicit def writes: Writes[RefRole] = Json.writes[RefRole]

  implicit def atomicWrites: Writes[RefRoleAtom] = Json.writes[RefRoleAtom]
}

object Authority extends UriGenerator[Authority] with JsonSerialisation[AuthorityProtocol, Authority] {
  import RefRole._

  def empty = Authority(UUID.randomUUID(), Set.empty, UUID.randomUUID())

  override def base: String = "authorities"

  override implicit def reads: Reads[AuthorityProtocol] = Json.reads[AuthorityProtocol]

  override implicit def writes: Writes[Authority] = Json.writes[Authority]

  implicit def writesAtomic: Writes[AuthorityAtom] = new Writes[AuthorityAtom] {
    override def writes(o: AuthorityAtom): JsValue = {
      def toJson(userjs: JsValue): JsValue = Json.obj(
        "user" -> userjs,
        "refRoles" -> JsArray((o.refRoles map (Json.toJson(_))).toSeq),
        "id" -> s"${o.id}"
      )
      o.user match {
        case student: Student => toJson(Json.toJson(student))
        case employee: Employee => toJson(Json.toJson(employee))
      }
    }
  }
}