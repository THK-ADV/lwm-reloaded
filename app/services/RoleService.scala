package services

import java.util.UUID

import models.Labwork
import models.security._
import models.users.User
import store.Prefixes.LWMPrefix
import store.SesameRepository
import store.bind.Bindings
import utils.Ops._
import utils.Ops.MonadInstances._
import utils.Ops.TraverseInstances._
import scala.util.{Success, Try}

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
  def checkWith(checkee: (Option[UUID], Set[Permission]))(checker: Set[UUID]): Try[Boolean]
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
    val useruri = User.generateUri(UUID.fromString(userId))
    val result = repository.query {
      select("auth") where {
        ^(v("auth"), p(lwm.privileged), s(useruri))
      }
    }.flatMap(_.get("auth"))

    for {
      values <- result
      first <- values.headOption
      authority <- repository.get[Authority](first.stringValue()).toOption.flatten
    } yield authority
  }

  override def checkWith(whatToCheck: (Option[UUID], Set[Permission]))(checkWith: Set[UUID]): Try[Boolean] = whatToCheck match {
    case (optLab, permissions) =>
      import bindings.RefRoleBinding._
      import bindings.RoleBinding._
      import bindings.LabworkBinding._

      repository.getMany[RefRole](checkWith map RefRole.generateUri) flatMap { refRoles =>
        if(refRoles.exists(_.role == Roles.admin.id)) Success(true)
        else for {
            optCourse <- Try(optLab).flatPeek (lab => repository.get[Labwork](Labwork.generateUri(lab))).peek(_.course)(tryM, optM)
            optRef = refRoles.find(_.module == optCourse)
            optRole <- Try(optRef) flatPeek (ref => repository.get[Role](Role.generateUri(ref.role)))
          } yield optRole exists (role => permissions forall role.permissions.contains)
      }
  }
}