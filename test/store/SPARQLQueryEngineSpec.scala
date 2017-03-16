package store

import java.util.UUID

import base.TestBaseDefinition
import models._
import org.openrdf.model.Value
import org.openrdf.repository.RepositoryConnection
import org.scalatest.WordSpec
import org.w3.banana.sesame.{Sesame, SesameModule}
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import store.sparql.{SPARQLQueryEngine, SelectClause}
import store.sparql.select._
import store.sparql.select

import scala.util.{Success, Try}

class SPARQLQueryEngineSpec extends WordSpec with TestBaseDefinition {

  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)

  val repo = SesameRepository(ns)

  val prefixes = LWMPrefix[repo.Rdf]

  val sparql = new SPARQLQueryEngine with SesameModule {
    override def connect[A](f: (RepositoryConnection) => Try[A]): Try[A] = repo.connect(f)
  }

  "A SPARQL engine" should {

    "execute select queries" in {
      import utils.Ops.MonadInstances.optM
      import utils.Ops.TraverseInstances.travO
      import utils.Ops.NaturalTrasformations._
      import bindings.StudentDescriptor

      val student = SesameStudent("mi1111", "Carl", "Heinz", "117272", "mi1111@gm.fh-koeln.de", SesameDegree.randomUUID)

      repo add student

      val query: SelectClause =
        select ("id") where {
          **(v("s"), p(prefixes.systemId), o(student.systemId)) .
          **(v("s"), p(prefixes.id), v("id"))
        }

      val result =
        sparql.prepareQuery(query).
        select(_.get("id")).
        transform(_.fold(List.empty[Value])(a => a)).
        transform(_.headOption).
        map(value => User.generateUri(UUID.fromString(value.stringValue())))(optM).
        request(repo.get[SesameStudent]).
        run

      result shouldBe Success(Some(student))
    }


    "transform higher kinds variably" in {
      import utils.Ops.FunctorInstances._
      import bindings.{
      StudentDescriptor,
      GroupDescriptor
      }

      val s1 = SesameStudent("mi1111", "Carl1", "Heinz", "117272", "mi1111@gm.fh-koeln.de", SesameDegree.randomUUID)
      val s2 = SesameStudent("mi1212", "Carl2", "Heinz", "177772", "mi1212@gm.fh-koeln.de", SesameDegree.randomUUID)
      val s3 = SesameStudent("mi1313", "Carl3", "Heinz", "171711", "mi1313@gm.fh-koeln.de", SesameDegree.randomUUID)
      val group = Group("A", UUID.randomUUID(), Set(s1.id, s2.id, s3.id))

      repo addMany List(s1, s2, s3)
      repo add group

      val query =
        select ("members") where {
          **(v("s"), p(prefixes.members), v("members")) .
          **(v("s"), p(prefixes.label), o(group.label))
        }

      val result = sparql.
        prepareQuery(query).
        select(_.get("members")).
        transform(_.fold(List.empty[Value])(identity)).
        transform(_.map(_.stringValue())).
        requestAll(repo.getMany[SesameStudent](_)).
        map(_.id).
        run

      result shouldBe Success(group.members)
    }

    "stop execution when an error occurs and propagate it" in {
      import utils.Ops.FunctorInstances._
      import bindings.{
      StudentDescriptor,
      GroupDescriptor
      }

      val s1 = SesameStudent("mi1111", "Carl1", "Heinz", "117272", "mi1111@gm.fh-koeln.de", SesameDegree.randomUUID)
      val s2 = SesameStudent("mi1212", "Carl2", "Heinz", "177772", "mi1212@gm.fh-koeln.de", SesameDegree.randomUUID)
      val group = Group("A", UUID.randomUUID(), Set(s1.id, s2.id, UUID.randomUUID()))

      repo addMany List(s1, s2)
      repo add group

      val query =
        select ("members") where {
          **(v("s"), p(prefixes.members), v("members")) .
            **(v("s"), p(prefixes.label), o(group.label))
        }

      val result = sparql.
        prepareQuery(query).
        select(_.get("members")).
        transform(_.fold(List.empty[Value])(identity)).
        transform(_.map(_.stringValue())).
        requestAll(repo.getMany[SesameStudent](_)).
        map(_.id).
        run

      result.isSuccess shouldBe true
      result.get.size shouldBe 2
    }

    "not run the query if one attempts to run it to soon" in {
      val query = select ("")

      val result = sparql.prepareQuery(query).run

      result match {
        case util.Success(_) => fail("Test should be a failure")
        case util.Failure(e) => e.getMessage shouldBe "Query cannot be run at this stage. Try selecting specific elements from the Map and then running it."
      }
    }
  }

  override protected def beforeEach() {
    repo.reset().foreach(r => assert(repo.size.get == 0))
  }

  override protected def beforeAll() {
    repo.reset().foreach(r => assert(repo.size.get == 0))
  }
}
