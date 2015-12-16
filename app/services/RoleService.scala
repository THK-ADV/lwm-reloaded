package services

import java.util.UUID

import models.Labwork
import models.security._
import store.Prefixes.LWMPrefix
import store.SesameRepository
import store.bind.Bindings

trait RoleServiceLike {

  /**
   * Retrieves the authority of a particular user.
    *
    * @param userId User ID
   * @return User's possible authority
   */
  def authorityFor(userId: String): Option[Authority]

  /**
   * Checks if the `checker` is allowed to pass the restrictions defined in `checkee`
    *
    * @param checkee restrictions
   * @param checker to be checked
   * @return true/false
   */
  def checkWith(checkee: (Option[UUID], Set[Permission]))(checker: Set[RefRole]): Boolean

  /**
   * Composition between `authorityFor` and `checkWith` functions.
   * Checks if a particular user is allowed to pass the restrictions defined in `checkee`
   *
   * @param checkee restrictions
   * @param userId User ID
   * @return true/false
   */
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

    val result = repository.query {
      select("auth") where {
        ^(v("auth"), p(lwm.privileged), o(userId))
      }
    }.flatMap(_.get("auth"))

    for {
      values <- result
      first <- values.headOption
      authority <- repository.get[Authority](first.stringValue()).toOption.flatten
    } yield authority
  }


  override def checkWith(checkee: (Option[UUID], Set[Permission]))(checker: Set[RefRole]): Boolean = checkee match {
    case (moduleCont, permissions) =>
      import bindings.RoleBinding._
      import bindings.LabworkBinding._
      checker.exists(_.role == Roles.admin.id) || {
        val course = moduleCont flatMap (u => repository.get[Labwork](Labwork.generateUri(u)).toOption.flatten.map(_.course))
        (for {
          ref <- checker.find(_.module == course)
          role <- repository.get[Role](Role.generateUri(ref.role)).toOption.flatten
        } yield permissions.forall(role.permissions.contains)) getOrElse false
      }
  }
}