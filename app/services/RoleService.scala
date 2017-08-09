package services

import java.util.UUID

import models._
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
  def authorities(userId: UUID): Try[Set[SesameAuthority]]

  /**
    * Checks if the `checker` is allowed to pass the restrictions defined in `checkee`
    *
    * @param checkee restrictions
    * @param checker to be checked
    * @return true/false
    */
  def checkAuthority(checkee: (Option[UUID], SesamePermission))(checker: SesameAuthority*): Try[Boolean]

  def rolesForCourse(userId: UUID): Try[Set[SesameRole]]
}

class RoleService(private val repository: SesameRepository) extends RoleServiceLike {

  import repository._

  private val lwm = LWMPrefix[Rdf]
  private val bindings = Bindings[Rdf](namespace)

  override def authorities(userId: UUID): Try[Set[SesameAuthority]] = {
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
      requestAll[Set, SesameAuthority](repository.getMany[SesameAuthority]).
      run
  }

  def rolesByLabel(labels: String*): Try[Set[SesameRole]] = {
    import bindings.RoleDescriptor

    repository.getAll[SesameRole].map(_.filter(role => labels contains role.label))
  }

  def rolesForCourse(userId: UUID): Try[Set[SesameRole]] = {
    import models.Roles.{CourseManagerLabel, RightsManagerLabel}

    for {
      userAuths <- authorities(userId)
      roles <- rolesByLabel(CourseManagerLabel, RightsManagerLabel)
    } yield roles.filterNot(role => role.label == RightsManagerLabel && userAuths.exists(_.role == role.id))
  }

  override def checkAuthority(whatToCheck: (Option[UUID], SesamePermission))(checkWith: SesameAuthority*): Try[Boolean] = whatToCheck match {
    case (_, permission) if permission == Permissions.god => Success(false)
    case (optCourse, permission) =>
      import bindings.RoleDescriptor

      def isAdmin(roles: Set[SesameRole]) = roles
        .find(_.label == Roles.AdminLabel)
        .exists { admin =>
          checkWith.exists(_.role == admin.id)
        }

      def rolesByCourse(roles: Set[SesameRole]) = checkWith
        .filter(_.course == optCourse)
        .flatMap { authority =>
          roles.filter(_.id == authority.role)
        }
        .toSet

      def hasPermission(roles: Set[SesameRole]) = roles.exists(_.permissions contains permission)

      repository.getAll[SesameRole] map { roles =>
        isAdmin(roles) || (rolesByCourse _ andThen hasPermission) (roles)
      }
  }
}