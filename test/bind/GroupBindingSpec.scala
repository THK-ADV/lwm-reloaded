package bind

import java.util.UUID

import base.SesameDbSpec
import models.labwork.{Group, GroupAtom, Labwork}
import models.users.{Student, User}
import org.w3.banana.PointedGraph

import scala.util.{Failure, Success}

class GroupBindingSpec extends SesameDbSpec {

  import bindings.{GroupDescriptor, dateTimeBinder, uuidBinder, uuidRefBinder}
  import ops._

  implicit val groupBinder = GroupDescriptor.binder

  val group = Group("Label", Labwork.randomUUID, Set(User.randomUUID, User.randomUUID))
  val groupGraph = URI(Group.generateUri(group)).a(lwm.Group)
    .--(lwm.label).->-(group.label)
    .--(lwm.labwork).->-(group.labwork)(ops, uuidRefBinder(Labwork.splitter))
    .--(lwm.members).->-(group.members)(ops, uuidRefBinder(User.splitter))
    .--(lwm.invalidated).->-(group.invalidated)
    .--(lwm.id).->-(group.id).graph

  "A GroupBindingSpec" should {

    "return a RDF graph representation of a group" in {
      val graph = group.toPG.graph

      graph isIsomorphicWith groupGraph shouldBe true
    }

    "return a group based on a RDF graph representation" in {
      val expectedGroup = PointedGraph[Rdf](URI(Group.generateUri(group)), groupGraph).as[Group]

      expectedGroup match {
        case Success(s) =>
          s shouldEqual group
        case Failure(e) =>
          fail(s"Unable to deserialise group graph: $e")
      }
    }

    "return an atomic group based on an RDF graph representation" in {
      import bindings.{GroupAtomDescriptor, GroupDescriptor, LabworkDescriptor, StudentDescriptor}

      val labwork = Labwork("Label", "Description", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
      val student1 = Student("systemid1", "lastname1", "firstname1", "email1", "registrationId1", UUID.randomUUID())
      val student2 = Student("systemid2", "lastname2", "firstname2", "email2", "registrationId2", UUID.randomUUID())
      val group = Group("label", labwork.id, Set(student1.id, student2.id))

      val groupAtom = GroupAtom(group.label, labwork, Set(student1, student2), group.invalidated, group.id)

      repo.add[Labwork](labwork)
      repo.add[Student](student1)
      repo.add[Student](student2)
      repo.add[Group](group)

      repo.get[GroupAtom](Group.generateUri(group.id)) match {
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
