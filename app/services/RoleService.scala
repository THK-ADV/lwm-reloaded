package services

import java.util.UUID

import models.security._
import models.users.User
import org.openrdf.model.Value
import store.Prefixes.LWMPrefix
import store.SesameRepository
import store.bind.Bindings
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
  def authorityFor(userId: UUID): Try[Set[Authority]]

  /**
    * Checks if the `checker` is allowed to pass the restrictions defined in `checkee`
    *
    * @param checkee restrictions
    * @param checker to be checked
    * @return true/false
    */
  def checkWith(checkee: (Option[UUID], Permission))(checker: Authority*): Try[Boolean]
}

class RoleService(private val repository: SesameRepository) extends RoleServiceLike {

  import repository._

  private val lwm = LWMPrefix[Rdf]
  private val bindings = Bindings[Rdf](namespace)

  override def authorityFor(userId: UUID): Try[Set[Authority]] = {
    import bindings.AuthorityDescriptor
    import store.sparql.select
    import store.sparql.select._

    val query = select("auth") where {
      **(v("auth"), p(lwm.privileged), s(User.generateUri(userId)))
    }

    repository.prepareQuery(query).
      select(_.get("auth")).
      transform(_.fold(List.empty[Value])(identity)).
      map(_.stringValue).
      requestAll[Set, Authority](repository.getMany[Authority]).
      run
  }

  def rolesByLabel(labels: String*): Try[Set[Role]] = {
    import bindings.RoleDescriptor

    repository.getAll[Role].map(_.filter(role => labels contains role.label))
  }

  override def checkWith(whatToCheck: (Option[UUID], Permission))(checkWith: Authority*): Try[Boolean] = whatToCheck match {
    case (_, permission) if permission == Permissions.god => Success(false)
    case (optCourse, permission) =>
      import bindings.RoleDescriptor

      def isAdmin(roles: Set[Role]) = {
        val adminRole = roles.find(_.label == Roles.Admin)
        checkWith.exists(auth => adminRole.exists(_.id == auth.role))
      }

      def rolesFilteredByCourse(roles: Set[Role]) = {
        val courseRelatedAuthorities = checkWith.filter(_.course == optCourse)
        roles.filter(role => courseRelatedAuthorities.exists(_.role == role.id))
      }

      def hasPermission(roles: Set[Role]) = roles.exists(_.permissions contains permission)

      repository.getAll[Role] map { roles =>
        if (isAdmin(roles))
          true
        else
          (rolesFilteredByCourse _ andThen hasPermission)(roles)
      }
  }
}