package bind.security

import base.SesameDbSpec
import models.Course
import models.security.{Authority, Permission, Role, RefRole}
import models.users.Student
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.Namespace
import store.bind.Bindings

import scala.util.Success

class AuthorityBindingSpec extends SesameDbSpec {

  import ops._

  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")
  val bindings = Bindings[Sesame](ns)

  import bindings.RefRoleBinding._
  import bindings.AuthorityBinding._
  import bindings.uuidBinder

  val student = Student("mi1234", "Doe", "John", "11234567", "mi1234@gm.fh-koeln.de", Student.randomUUID)

  val authorityForCourse1 = RefRole(
    Some(Course.randomUUID),
    Role("role", Set(Permission("create"))),
    RefRole.randomUUID
  )

  val authorityForCourse2 = RefRole(
    Some(Course.randomUUID),
    Role("role", Set(Permission("delete"), Permission("view"))),
    RefRole.randomUUID
  )

  val authWith = Authority(student.id, Set(authorityForCourse1, authorityForCourse2), Authority.randomUUID)
  val authWithout = Authority(student.id, Set.empty[RefRole], Authority.randomUUID)

  val authorityGraph = (
    URI(Authority.generateUri(authWith)).a(lwm.Authority)
      -- lwm.id ->- authWith.id
      -- lwm.privileged ->- authWith.user
      -- lwm.refroles ->- authWith.refRoles
    ).graph

  "An authority" should {
    "return a RDF graph representation of an Authority" in {
      val graph = authWith.toPG.graph

      graph isIsomorphicWith authorityGraph shouldBe true
    }

    "return an Authority representation of an RDF graph" in {
      val graph = authWith.toPG.graph
      val authConverted = PointedGraph[Rdf](URI(Authority.generateUri(authWith)), graph).as[Authority]

      authConverted match {
        case Success(des) => des shouldBe authWith
        case _ => fail("Graph -> Authority morphism failed")
      }
    }

    "return a RDF graph representation of an Authority with empty authorization" in {
      val graph = authWithout.toPG.graph
      val authConverted = PointedGraph[Rdf](URI(Authority.generateUri(authWithout)), graph).as[Authority]

      authConverted match {
        case Success(des) =>
          des shouldBe authWithout
          des.refRoles.isEmpty shouldBe true
        case _ => fail("Graph -> Authority morphism failed")
      }
    }
  }

}
