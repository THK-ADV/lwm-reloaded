package services

import security.RefRole


trait RoleServiceLike[A] {
  def permissionsFor(systemId: String): Set[A]
}

class RoleService extends RoleServiceLike[RefRole] {
  def permissionsFor(systemId: String): Set[RefRole] = ???
}
