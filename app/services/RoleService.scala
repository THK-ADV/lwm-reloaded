package services

import java.util.UUID

import models.security.{Permission, Roles, Authority, RefRole}
import store.Prefixes.LWMPrefix
import store.SesameRepository
import store.bind.Bindings

trait RoleServiceLike {

  def authorityFor(userId: String): Option[Authority]

  def checkWith(checkee: (Option[UUID], Set[Permission]))(checker: Set[RefRole]): Boolean

  def checkFor(checkee: (Option[UUID], Set[Permission]))(userId: String): Boolean = authorityFor(userId) exists (e => checkWith(checkee)(e.refRoles))
}

class RoleService(repository: SesameRepository) extends RoleServiceLike {

  import repository._

  private val lwm = LWMPrefix[Rdf]
  private val bindings = Bindings[Rdf](namespace)

  override def authorityFor(userId: String): Option[Authority] = {
    import store.sparql.select
    import store.sparql.select._
    import bindings.AuthorityBinding._
    import bindings.RefRoleBinding._

    repository.query {
      select("auth") where {
        ^(v("auth"), p(lwm.privileged), o(userId))
      }
    }.flatMap(_.get("auth")).flatMap(v => get[Authority](v.stringValue()).toOption.flatten)
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
  override def checkWith(checkee: (Option[UUID], Set[Permission]))(checker: Set[RefRole]): Boolean = checkee match {
    case (module, permissions) => val res = checker.find(_.module == checkee._1) match {
        case Some(ref) => permissions.forall(ref.role.permissions.contains)
        case None => checker.exists(_.role == Roles.admin)
      }
    println(s"-----------------------------------------------CHECKEE: $checkee :: CHECKER: $checker")
    println(s"-----------------------------------------------RESULT: $res")
    res
  }
}