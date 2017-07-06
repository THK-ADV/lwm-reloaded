package bind

import java.util.UUID

import base.SesameDbSpec
import models._
import org.w3.banana.PointedGraph

import scala.util.{Failure, Success}

class GroupBindingSpec extends SesameDbSpec {

  import bindings.{GroupDescriptor, dateTimeBinder, uuidBinder, uuidRefBinder}
  import ops._

  implicit val groupBinder = GroupDescriptor.binder

  val group = SesameGroup("Label", SesameLabwork.randomUUID, Set(User.randomUUID, User.randomUUID))
  val groupGraph = URI(SesameGroup.generateUri(group)).a(lwm.Group)
    .--(lwm.label).->-(group.label)
    .--(lwm.labwork).->-(group.labwork)(ops, uuidRefBinder(SesameLabwork.splitter))
    .--(lwm.members).->-(group.members)(ops, uuidRefBinder(User.splitter))
    .--(lwm.invalidated).->-(group.invalidated)
    .--(lwm.id).->-(group.id).graph

  "A GroupBindingSpec" should {

    "return a RDF graph representation of a group" in {
      val graph = group.toPG.graph

      graph isIsomorphicWith groupGraph shouldBe true
    }

    "return a group based on a RDF graph representation" in {
      val expectedGroup = PointedGraph[Rdf](URI(SesameGroup.generateUri(group)), groupGraph).as[SesameGroup]

      expectedGroup match {
        case Success(s) =>
          s shouldEqual group
        case Failure(e) =>
          fail(s"Unable to deserialise group graph: $e")
      }
    }

    "return an atomic group based on an RDF graph representation" in {
      import bindings.{GroupAtomDescriptor, GroupDescriptor, LabworkDescriptor, StudentDescriptor}

      val labwork = SesameLabwork("Label", "Description", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
      val student1 = SesameStudent("systemid1", "lastname1", "firstname1", "email1", "registrationId1", UUID.randomUUID())
      val student2 = SesameStudent("systemid2", "lastname2", "firstname2", "email2", "registrationId2", UUID.randomUUID())
      val group = SesameGroup("label", labwork.id, Set(student1.id, student2.id))

      val groupAtom = SesameGroupAtom(group.label, labwork, Set(student1, student2), group.invalidated, group.id)

      repo.add[SesameLabwork](labwork)
      repo.add[SesameStudent](student1)
      repo.add[SesameStudent](student2)
      repo.add[SesameGroup](group)

      repo.get[SesameGroupAtom](SesameGroup.generateUri(group.id)) match {
        case Success(Some(dgroup)) =>
          dgroup shouldEqual groupAtom
        case Success(None) =>
          fail("Groups were not found")
        case Failure(e) =>
          fail(s"Group could not be deserialised: $e")

      }
    }
  }
}
