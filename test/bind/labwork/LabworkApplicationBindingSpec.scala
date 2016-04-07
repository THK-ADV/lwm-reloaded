package bind.labwork

import base.SesameDbSpec
import models.labwork.{Labwork, LabworkApplication}
import models.users.User
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.{Failure, Success}

class LabworkApplicationBindingSpec extends SesameDbSpec {

  import ops._
  val bindings = Bindings[Sesame](repo.namespace)

  import bindings.LabworkApplicationBinding._
  import bindings.{uuidBinder, uuidRefBinder, dateTimeBinder}

  val student = User.randomUUID
  val friend1 = User.randomUUID
  val friend2 = User.randomUUID
  val friend3 = User.randomUUID
  val application = LabworkApplication(Labwork.randomUUID, student, Set(friend1, friend2))

  val applicationGraph = URI(LabworkApplication.generateUri(application)).a(lwm.LabworkApplication)
    .--(lwm.labwork).->-(application.labwork)(ops, uuidRefBinder(Labwork.splitter))
    .--(lwm.applicant).->-(application.applicant)(ops, uuidRefBinder(User.splitter))
    .--(lwm.timestamp).->-(application.timestamp)
    .--(lwm.friends).->-(application.friends)(ops, uuidRefBinder(User.splitter))
    .--(lwm.id).->-(application.id).graph

  "A LabworkApplicationBinding" should {
    "return a RDF graph representation of a labwork application" in {
      val graph = application.toPG.graph

      graph isIsomorphicWith applicationGraph shouldBe true
    }

    "return an labwork application based on an RDF representation" in {
      val expectedLabworkApplication = PointedGraph[Rdf](URI(LabworkApplication.generateUri(application)), applicationGraph).as[LabworkApplication]

      expectedLabworkApplication match {
        case Success(s) =>
          s.applicant shouldEqual application.applicant
          s.id shouldEqual application.id
          s.friends shouldEqual application.friends
          s.labwork shouldEqual application.labwork
          s.timestamp.isEqual(application.timestamp) shouldBe true
        case Failure(e) =>
          fail(s"Unable to deserialise labwork application graph: $e")
      }
    }

  }
}
