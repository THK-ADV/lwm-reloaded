package security

import java.util.UUID

case class Role(name: String, permissions: Set[Permission])

case class RefRole(module: Option[UUID] = None, role: Role)

case class Permission(get: String)

object Permissions {

}
