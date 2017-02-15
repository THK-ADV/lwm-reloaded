package store

import java.util.UUID

import models._
import org.w3.banana.{PointedGraph, RDFPrefix}
import org.w3.banana.sesame.{Sesame, SesameModule}
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import store.bind.Descriptor.Descriptor
import store.sparql.select
import store.sparql.select._
import utils.Ops.MonadInstances.optM
import utils.Ops.NaturalTrasformations._

import scala.util.{Failure, Success, Try}

trait Resolvers {

  def userId(systemId: String): Try[Option[UUID]]

  def missingUserData[A <: User](v: A): Try[PointedGraph[Sesame]]

  def degree(abbreviation: String): Try[UUID]
}

class LwmResolvers(val repository: SesameRepository) extends Resolvers {

  import repository._

  val prefix = LWMPrefix[Sesame]
  val rdf = RDFPrefix[Sesame]
  val bindings = Bindings(repository.namespace)

  override def userId(systemId: String): Try[Option[UUID]] = {
    val query = select("id") where {
      **(v("s"), p(prefix.systemId), o(systemId)).
        **(v("s"), p(prefix.id), v("id"))
    }

    repository.prepareQuery(query).
      select(_.get("id")).
      changeTo(_.headOption).
      map(value => UUID.fromString(value.stringValue())).
      run
  }

  /**
    * missingUserData hat zwei verantwortlichkeiten
    * erstens, es sucht die passende rolle und fügt eine authority hinzu
    * zweitens, es füngt den user als solches hinzu
    * das ist verwirrend, es wäre besser, wenn ldap.user(user)(degree) KEINEN lwm.models.user zurückgibt, sondern einen LdapUser
    * dieser LdapUser wird mit missingUserData um eine Auth und dem Degree angereichert. Danach werden auth und user in die db geworfen
    * beispiel: ldap.user(user) map (missingUserData) map (createAuthAndUser)
    * dandruch wird auch die degree funtkion aus dem ldap wegoptimiert. die hat auch nichts dazu zu tun, das sind lwm details
    */
  override def missingUserData[A <: User](user: A): Try[PointedGraph[Sesame]] = {
    import bindings.{AuthorityDescriptor, RoleDescriptor, StudentDescriptor, EmployeeDescriptor}

    def createAuthAndUser[B <: User](entity: B)(p: Role => Boolean)(implicit descriptor: Descriptor[Rdf, B]) = {
      repository.getAll[Role] flatMap { roles =>
        roles.find(p) match {
          case Some(role) =>
            for {
              userPg <- repository.add[B](entity)
              _ <- repository.add[Authority](Authority(entity.id, role.id))
            } yield userPg
          case None =>
            Failure(new Throwable("No appropriate RefRole or Role found while resolving user"))
        }
      }
    }

    user match {
      case s: SesameStudent => createAuthAndUser(s)(_.label == Roles.Student)
      case e: SesameEmployee => createAuthAndUser(e)(_.label == Roles.Employee)
    }
  }

  override def degree(abbreviation: String): Try[UUID] = {
    import bindings.DegreeDescriptor
    import utils.Ops.MonadInstances.{tryM, optM}
    import utils.Ops.TraverseInstances.travO

    val query = select ("degree") where {
      **(v("degree"), p(rdf.`type`), s(prefix.Degree)).
      **(v("degree"), p(prefix.abbreviation), o(abbreviation))
    }

    repository.prepareQuery(query).
      select(_.get("degree")).
      changeTo(_.headOption).
      request[Option, SesameDegree](value => repository.get[SesameDegree](value.stringValue())).
      transform(_.fold[Try[SesameDegree]](Failure(new Throwable(s"No viable degree found for abbreviation $abbreviation")))(Success(_))).
      map(_.id).
      run.
      flatten
  }
}
