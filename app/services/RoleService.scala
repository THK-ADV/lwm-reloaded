package services

import security.{Permission, RefRole}
import store.SemanticRepository


trait RoleServiceLike[A] {
  def permissionsFor(systemId: String): Set[A]

  def checkWith(checkee: Set[A])(checker: Set[A]): Boolean

  def checkFor(checkee: Set[A])(systemId: String) = checkWith(checkee)(permissionsFor(systemId))
}

class RoleService(repository: SemanticRepository) extends RoleServiceLike[RefRole] {
  override def permissionsFor(systemId: String): Set[RefRole] = ???

  /*
   * TODO: Possible optimization
   * In a situation like the following:
   *
   *  _  -> StudentPerms
   *  _  -> EmployeePerms
   *  ...
   *
   * Instead of checking if one of n similar categories contains all permissions, we might simply merge them both and check if
   * the current slice is contained in their merger. As both of them are, technically, part of the same domain, their merger
   * should not reveal any side effects. (A => A => A)
   */
  override def checkWith(checkee: Set[RefRole])(checker: Set[RefRole]): Boolean = {
    checkee.forall { r =>
      checker.filter(_.module == r.module)
        .find(e => r.role.permissions.forall(e.role.permissions.contains)) match {
        case Some(ref) => true
        case _ => false
      }
    }
  }

}