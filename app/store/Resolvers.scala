package store

import java.util.UUID

import models.security.{Authority, RefRole, Roles}
import models.users.{Employee, Student, User}
import org.openrdf.model.Value
import org.w3.banana.PointedGraph
import org.w3.banana.binder.ToPG
import org.w3.banana.sesame.{Sesame, SesameModule}
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import store.sparql.select
import store.sparql.select._
import utils.Ops.MonadInstances.optM
import utils.Ops.NaturalTrasformations._
import scala.util.{Failure, Try}

trait Resolvers {
  type R <: org.w3.banana.RDF

  def username(systemId: String): Try[Option[UUID]]

  def missingUserData[A <: User](v: A): Try[PointedGraph[R]]
}

class LwmResolvers(val repository: SesameRepository) extends Resolvers {

  import repository._

  override type R = SesameModule#Rdf
  val prefix = LWMPrefix[Sesame]
  val bindings = Bindings(repository.namespace)

  override def username(systemId: String): Try[Option[UUID]] = {
    val result = repository.prepareQuery {
      select("id") where {
          ^(v("s"), p(prefix.systemId), o(systemId)).
          ^(v("s"), p(prefix.id), v("id"))
      }
    }

    result.
      select(_.get("id")).
      changeTo(_.headOption).
      map(value => UUID.fromString(value.stringValue())).
      run
  }

  override def missingUserData[A <: User](v: A): Try[PointedGraph[Sesame]] = {
    import bindings.RefRoleBinding._
    def f[Z <: User](entity: Z)(p: RefRole => Boolean)(implicit serialiser: ToPG[Sesame, Z]): Try[PointedGraph[Sesame]] =
      for {
        refroles <- repository.get[RefRole]
        filtered = refroles.find(p)
        user <- filtered match {
          case Some(refRole) =>
            import bindings.AuthorityBinding._
            for {
              user <- repository.add[Z](entity)(serialiser)
              _ <- repository.add[Authority](Authority(v.id, Set(refRole.id)))
            } yield user
          case _ => Failure(new Throwable("No appropriate RefRole found while resolving user"))
        }
      } yield user

    v match {
      case s: Student => f(s)(_.role == Roles.student.id)(bindings.StudentBinding.studentBinder)
      case e: Employee => f(e)(_.role == Roles.user.id)(bindings.EmployeeBinding.employeeBinder)
    }
  }

}
