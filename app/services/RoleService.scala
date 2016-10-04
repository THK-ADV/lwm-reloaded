package services

import java.util.UUID

import models.security._
import models.users.User
import org.openrdf.model.Value
import store.Prefixes.LWMPrefix
import store.SesameRepository
import store.bind.Bindings
import utils.Ops.MonadInstances._

import scala.util.{Success, Try}

trait RoleServiceLike {

  /**
    * Retrieves the authorities of a particular user.
    *
    * @param userId User ID
    * @return User's possible authorities
    */
  def authorities(userId: UUID): Try[Set[Authority]]

  /**
    * Checks if the `checker` is allowed to pass the restrictions defined in `checkee`
    *
    * @param checkee restrictions
    * @param checker to be checked
    * @return true/false
    */
  def checkAuthority(checkee: (Option[UUID], Permission))(checker: Authority*): Try[Boolean]
}

class RoleService(private val repository: SesameRepository) extends RoleServiceLike {

  import repository._

  private val lwm = LWMPrefix[Rdf]
  private val bindings = Bindings[Rdf](namespace)

  override def authorities(userId: UUID): Try[Set[Authority]] = {
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

  def rolesForCourse(userId: UUID): Try[Set[Role]] = {
    import models.security.Roles.{CourseManager, RightsManager}

    for {
      userAuths <- authorities(userId)
      roles <- rolesByLabel(CourseManager, RightsManager)
    } yield roles.filterNot(role => role.label == RightsManager && userAuths.exists(_.role == role.id))
  }

  override def checkAuthority(whatToCheck: (Option[UUID], Permission))(checkWith: Authority*): Try[Boolean] = whatToCheck match {
    case (_, permission) if permission == Permissions.god => Success(false)
    case (optCourse, permission) =>
      import bindings.RoleDescriptor

      def isAdmin(roles: Set[Role]) = roles
        .find(_.label == Roles.Admin)
        .exists { admin =>
          checkWith.exists(_.role == admin.id)
        }

      def rolesByCourse(roles: Set[Role]) = checkWith
        .filter(_.course == optCourse)
        .flatMap { authority =>
          roles.filter(_.id == authority.role)
        }
        .toSet

      def hasPermission(roles: Set[Role]) = roles.exists(_.permissions contains permission)

      repository.getAll[Role] map { roles =>
        isAdmin(roles) || (rolesByCourse _ andThen hasPermission) (roles)
      }
  }

}