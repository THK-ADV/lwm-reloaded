package bind.security

import base.SesameDbSpec
import models.security.{Permission, Role}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.Success

class RoleBindingSpec extends SesameDbSpec {

  val bindings = Bindings[Sesame](namespace)

  val roleWith = Role("role1", Set(Permission("p1"), Permission("p2"), Permission("p3")))
  val roleWithout = Role("role1", Set())

  import bindings.RoleBinding.roleBinder
  import bindings.permissionBinder
  import bindings.uuidBinder
  import ops._

  val roleGraph = (
    URI(Role.generateUri(roleWith)).a(lwm.Role)
      -- lwm.id ->- roleWith.id
      -- lwm.label ->- roleWith.label
      -- lwm.permissions ->- roleWith.permissions
    ).graph

  "A Role" should {
    "return a RDF graph representation of a Role" in {
      val graph = roleWith.toPG.graph

      graph isIsomorphicWith roleGraph shouldBe true
    }

    "return a Role representation of an RDF graph" in {
      val graph = roleWith.toPG.graph
      val authConverted = PointedGraph[Rdf](URI(Role.generateUri(roleWith)), graph).as[Role]

      authConverted match {
        case Success(des) => des shouldBe roleWith
        case _ => fail("Graph -> Authority morphism failed")
      }
    }

    "return a RDF graph representation of a Role with no permissions" in {
      val graph = roleWithout.toPG.graph
      val authConverted = PointedGraph[Rdf](URI(Role.generateUri(roleWithout)), graph).as[Role]

      authConverted match {
        case Success(des) =>
          des shouldBe roleWithout
          des.permissions.isEmpty shouldBe true
        case _ => fail("Graph -> Authority morphism failed")
      }
    }
  }

}
