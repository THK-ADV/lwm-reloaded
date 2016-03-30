package bind.labwork

import base.SesameDbSpec
import models.labwork.{Group, Labwork}
import models.users.User
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.{Failure, Success}

class GroupBindingSpec extends SesameDbSpec {

  import ops._

  val bindings = Bindings[Sesame](namespace)
  import bindings.GroupBinding.groupBinder
  import bindings.{uuidBinder, uuidRefBinder}

  val group = Group("Label", Labwork.randomUUID, Set(User.randomUUID, User.randomUUID), Group.randomUUID)
  val groupGraph = URI(Group.generateUri(group)).a(lwm.Group)
    .--(lwm.label).->-(group.label)
    .--(lwm.labwork).->-(group.labwork)(ops, uuidRefBinder(Labwork.splitter))
    .--(lwm.members).->-(group.members)(ops, uuidRefBinder(User.splitter))
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
  }
}
