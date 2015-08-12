package services

import models.security.RefRole
import store.Prefixes.LWMPrefix
import store.SesameRepository
import store.bind.Bindings
import utils.Ops._

trait RoleServiceLike[A] {
  def permissionsFor(systemId: String): Set[A]

  def checkWith(checkee: Set[A])(checker: Set[A]): Boolean

  def checkFor(checkee: Set[A])(systemId: String) = checkWith(checkee)(permissionsFor(systemId))
}

class RoleService(repository: SesameRepository) extends RoleServiceLike[RefRole] {
  override def permissionsFor(username: String): Set[RefRole] = {
    import repository._
    val prefix = LWMPrefix[Rdf]
    val bindings = Bindings[Rdf](namespace)
    import bindings.RefRoleBinding._

    select.map { v =>
      sequence(
        v.flatMap { bs =>
          get[RefRole](bs.getValue("refRoles").stringValue()).toOption
        }
      )
    } >>
      s"""
        | Select ?refRoles where {
        | ?s ${resource(prefix.systemId)} ${literal(username)} .
        | ?s ${resource(prefix.refroles)} ?refRoles
        | }
      """.stripMargin match {
      case Some(r) => r map (_.toSet) getOrElse Set.empty[RefRole]
      case _ => Set.empty[RefRole]
    }
  }

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