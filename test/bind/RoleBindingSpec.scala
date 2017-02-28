package bind

import base.SesameDbSpec
import models._
import org.w3.banana.PointedGraph

import scala.util.Success

class RoleBindingSpec extends SesameDbSpec {

  val roleWith = SesameRole("role1", Set(Permission("p1"), Permission("p2"), Permission("p3")))
  val roleWithout = SesameRole("role1", Set())

  import bindings.{RoleDescriptor, dateTimeBinder, permissionBinder, uuidBinder}
  import ops._

  implicit val roleBinder = RoleDescriptor.binder

  val roleGraph = (
    URI(SesameRole.generateUri(roleWith)).a(lwm.Role)
      -- lwm.label ->- roleWith.label
      -- lwm.permissions ->- roleWith.permissions
      -- lwm.id ->- roleWith.id
      -- lwm.invalidated ->- roleWith.invalidated
    ).graph

  "A Role" should {
    "return a RDF graph representation of a Role" in {
      val graph = roleWith.toPG.graph

      graph isIsomorphicWith roleGraph shouldBe true
    }

    "return a Role representation of an RDF graph" in {
      val graph = roleWith.toPG.graph
      val authConverted = PointedGraph[Rdf](URI(SesameRole.generateUri(roleWith)), graph).as[SesameRole]

      authConverted match {
        case Success(des) => des shouldBe roleWith
        case _ => fail("Graph -> Authority morphism failed")
      }
    }

    "return a RDF graph representation of a Role with no permissions" in {
      val graph = roleWithout.toPG.graph
      val authConverted = PointedGraph[Rdf](URI(SesameRole.generateUri(roleWithout)), graph).as[SesameRole]

      authConverted match {
        case Success(des) =>
          des shouldBe roleWithout
          des.permissions.isEmpty shouldBe true
        case _ => fail("Graph -> Authority morphism failed")
      }
    }
  }

}
