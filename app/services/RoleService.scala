package services

import java.util.UUID

import models.security._
import models.users.User
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import store.Prefixes.LWMPrefix
import store.SesameRepository
import store.bind.Bindings
import store.sparql.NoneClause
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
  def authorityFor(userId: String): Try[Option[Authority]]

  /**
    * Checks if the `checker` is allowed to pass the restrictions defined in `checkee`
    *
    * @param checkee restrictions
    * @param checker to be checked
    * @return true/false
    */
  def checkWith(checkee: (Option[UUID], Permission))(checker: Authority): Try[Boolean]
}

class RoleService(repository: SesameRepository) extends RoleServiceLike {

  import repository._

  private val lwm = LWMPrefix[Rdf]
  private val bindings = Bindings[Rdf](namespace)


  override def authorityFor(userId: String): Try[Option[Authority]] = {
    import store.sparql.select
    import store.sparql.select._
    import utils.Ops.NaturalTrasformations._
    import bindings.AuthorityDescriptor

    val useruri = User.generateUri(UUID.fromString(userId))
    val result = repository.prepareQuery {
      select("auth") where {
        **(v("auth"), p(lwm.privileged), s(useruri))
      }
    }

    result.
      select(_.get("auth")).
      changeTo(_.headOption).
      request(uri => repository.get[Authority](uri.stringValue())).
      run
  }

  def rolesByLabel(labels: String*): Try[Set[Role]] = {
    import bindings.RoleDescriptor
    repository.getAll[Role]
      .map(_ filter (role => labels contains role.label))
  }

  def refRolesByLabel(roleLabels: String*): Try[Set[RefRole]] = {
    import store.sparql.select
    import store.sparql.select._
    import bindings.RefRoleDescriptor

    lazy val rdf = RDFPrefix[repository.Rdf]

    val refroles =
      **(v("refrole"), p(rdf.`type`), s(lwm.RefRole)).
        **(v("refrole"), p(lwm.role), v("role"))

    val query = select ("refrole") where {
      roleLabels.foldLeft(refroles) { (clause, label) =>
        clause.optional {
          **(s("role"), p(lwm.label), o(label))
        }
      }
    }

    repository.prepareQuery(query)
      .select(_.get("refrole"))
      .transform(_.fold(List.empty[Value])(identity))
      .map(_.stringValue())
      .requestAll(uris => repository.getMany[RefRole](uris))
      .run
  }

  def refRoleByLabel(roleLabel: String): Try[Option[RefRole]] = refRolesByLabel(roleLabel) map (_.headOption)

  override def checkWith(whatToCheck: (Option[UUID], Permission))(checkWith: Authority): Try[Boolean] = whatToCheck match {
    case (optCourse, permission) if permission == Permissions.god => Success(false)
    case (optCourse, permission) =>
      import bindings.RefRoleDescriptor
      import bindings.RoleDescriptor

      (for {
        roles <- repository.getAll[Role]
        admin = roles.filter(_.label == Roles.Admin)
        refRoles <- repository.getMany[RefRole](checkWith.refRoles map RefRole.generateUri)
      } yield {
        if (refRoles.exists(refrole => admin.exists(_.id == refrole.role))) Success(true)
        else for {
          optRef <- Try(refRoles.filter(_.course == optCourse))
          optRole <- Try(optRef) flatMap (ref => repository.getMany[Role](ref.map(rr => Role.generateUri(rr.role))))
        } yield optRole.exists(_.permissions.contains(permission))
      }).flatten
  }
}