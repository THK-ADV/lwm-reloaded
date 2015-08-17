package services

import models.security.{ContextualRole, Authority, RefRole}
import store.Prefixes.LWMPrefix
import store.SesameRepository
import store.bind.Bindings
import utils.Ops._

trait RoleServiceLike {

  def authorityFor(userId: String): Option[Authority]

  def checkWith(checkee: Set[RefRole])(checker: Set[RefRole]): Boolean

  def checkFor(checkee: Set[RefRole])(userId: String): Boolean = authorityFor(userId) exists (e => checkWith(checkee)(e.refRoles))
}

class RoleService(repository: SesameRepository) extends RoleServiceLike {

  import repository._
  private val lwm = LWMPrefix[Rdf]
  private val bindings = Bindings[Rdf](namespace)
  import bindings.AuthorityBinding._

  override def authorityFor(userId: String): Option[Authority] = get[Authority].map (_.find(_.id.toString == userId)).toOption.flatten

  /*
   * TODO: Possible optimization
   * In a situation like the following:
   *
   *  _  -> StudentPerms
   *  _  -> EmployeePerms
   *  ...
   *
   * Instead of checking if one of n similar categories contains all permissions, we might simply merge all of them and check if
   * the current slice is contained in their merger. As all of them are, technically, part of the same domain, their merger
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