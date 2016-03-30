package bind.security

import base.SesameDbSpec
import models.{Course, Degree}
import models.security.{Authority, RefRole, Role}
import models.users.{Student, User}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.Success

class AuthorityBindingSpec extends SesameDbSpec {

  val bindings = Bindings[Sesame](namespace)

  import ops._
  import bindings.AuthorityBinding.authorityBinder
  import bindings.{uuidBinder, uuidRefBinder}

  val student = Student("mi1234", "Doe", "John", "11234567", "mi1234@gm.fh-koeln.de", Degree.randomUUID)

  val authorityForCourse1 = RefRole(Some(Course.randomUUID), Role.randomUUID, RefRole.randomUUID)

  val authorityForCourse2 = RefRole(Some(Course.randomUUID), Role.randomUUID, RefRole.randomUUID)

  val authWith = Authority(student.id, Set(authorityForCourse1.id, authorityForCourse2.id), Authority.randomUUID)
  val authWithout = Authority(student.id, Set.empty, Authority.randomUUID)

  val authorityGraph = URI(Authority.generateUri(authWith)).a(lwm.Authority)
    .--(lwm.id).->-(authWith.id)
    .--(lwm.privileged).->-(authWith.user)(ops, uuidRefBinder(User.splitter))
    .--(lwm.refroles).->-(authWith.refRoles)(ops, uuidRefBinder(RefRole.splitter)).graph

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
