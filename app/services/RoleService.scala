package services

import security.{Permission, RefRole}
import store.SemanticRepository


trait RoleServiceLike[A] {
  def permissionsFor(systemId: String): Set[A]
  def checkWith(checkee: Set[A])(checker: Set[A]): Boolean
  def chechFor(checkee: Set[A])(systemId: String) = checkWith(checkee)(permissionsFor(systemId))
}

class RoleService(/*repository: SemanticRepository*/) extends RoleServiceLike[RefRole] {
  override def permissionsFor(systemId: String): Set[RefRole] = ???

  override def checkWith(checkee: Set[RefRole])(checker: Set[RefRole]): Boolean = {
      (for {
        me <- checker.find(r => checkee map (_.module) contains r.module)
        him <- checkee.find(r => r.module == me.module)
      } yield {
          him.role.permissions.forall(me.role.permissions.contains)
        }) getOrElse false
  }
}
