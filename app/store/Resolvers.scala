package store

import java.util.UUID

import models.security.{Roles, RefRole, Role, Authority}
import models.users.{Employee, Student, User}
import org.w3.banana.PointedGraph
import org.w3.banana.binder.ToPG
import org.w3.banana.sesame.{SesameModule, Sesame}
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import store.sparql.select
import store.sparql.select._

import scala.util.{Failure, Try}

trait Resolvers {
  type R <: org.w3.banana.RDF

  def username(systemId: String): Option[UUID]

  def missingUserData[A <: User](v: A): Try[PointedGraph[R]]
}

class LwmResolvers(val repository: SesameRepository) extends Resolvers {

  import repository._

  override type R = SesameModule#Rdf
  val prefix = LWMPrefix[Sesame]
  val bindings = Bindings(repository.namespace)

  override def username(systemId: String): Option[UUID] = {
    repository.query {
      select("id") where {
        ^(v("s"), p(prefix.systemId), o(systemId)).
          ^(v("s"), p(prefix.id), v("id"))
      }
    }.flatMap(_.get("id")).map(v => UUID.fromString(v.stringValue()))
  }


  override def missingUserData[A <: User](v: A): Try[PointedGraph[Sesame]] = {
    import bindings.RoleBinding._
    withConnection(conn =>
    rdfStore.getGraph(conn, ns).map(println)
    )
    def f[Z <: User](e: Z)(implicit serialiser: ToPG[Sesame, Z]): Try[PointedGraph[Sesame]] =
      for {
        roles <- repository.get[Role]
        filtered = roles.find(_.name.toLowerCase == Roles.student.name.toLowerCase)
        user <- filtered match {
          case Some(role) =>
            import bindings.AuthorityBinding._
            for {
              user <- repository.add[Z](e)(serialiser)
              _ <- repository.add[Authority](Authority(v.id, Set(RefRole(None, role.id))))
            } yield user
          case _ =>
            println(roles)
            Failure(new Throwable("No student role found while resolving user"))
        }
      } yield user

    v match {
      case s: Student => f(s)(bindings.StudentBinding.studentBinder)
      case e: Employee => f(e)(bindings.EmployeeBinding.employeeBinder)
    }
  }

}
