package bind.security

import base.SesameDbSpec
import models.Course
import models.security.{Role, RefRole}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.{Failure, Success}

class RefRoleBindingSpec extends SesameDbSpec {

  val bindings = Bindings[Sesame](namespace)
  import bindings.RefRoleBinding.refRoleBinder
  import bindings.{uuidBinder, uuidRefBinder}
  import ops._

  val refRoleWithCourse = RefRole(Some(Course.randomUUID), Role.randomUUID, RefRole.randomUUID)

  val refRoleWithoutCourse = RefRole(None, Role.randomUUID, RefRole.randomUUID)

  val refRoleGraphWithCourse = URI(RefRole.generateUri(refRoleWithCourse)).a(lwm.RefRole)
    .--(lwm.course).->-(refRoleWithCourse.course)(ops, uuidRefBinder(Course.splitter))
    .--(lwm.role).->-(refRoleWithCourse.role)(ops, uuidRefBinder(Role.splitter))
    .--(lwm.id).->-(refRoleWithCourse.id).graph

  val refRoleGraphWithoutCourse = URI(RefRole.generateUri(refRoleWithoutCourse)).a(lwm.RefRole)
    .--(lwm.course).->-(refRoleWithoutCourse.course)(ops, uuidRefBinder(Course.splitter))
    .--(lwm.role).->-(refRoleWithoutCourse.role)(ops, uuidRefBinder(Role.splitter))
    .--(lwm.id).->-(refRoleWithoutCourse.id).graph

  "A RefRoleBindingSpec" should {

    "return a RDF graph representation of a refRole" in {
      val graph = refRoleWithCourse.toPG.graph

      graph isIsomorphicWith refRoleGraphWithCourse shouldBe true
    }

    "return a refRole based on a RDF graph representation" in {
      val expectedRefRole = PointedGraph[Rdf](URI(RefRole.generateUri(refRoleWithCourse)), refRoleGraphWithCourse).as[RefRole]

      expectedRefRole match {
        case Success(s) =>
          s shouldEqual refRoleWithCourse
        case Failure(e) =>
          fail(s"Unable to deserialise refRole graph: $e")
      }
    }

    "return a RDF graph representation of a refRole without course association" in {
      val graph = refRoleWithoutCourse.toPG.graph

      graph isIsomorphicWith refRoleGraphWithoutCourse shouldBe true
    }

    "return a refRole based on a RDF graph representation without course association" in {
      val expectedRefRole = PointedGraph[Rdf](URI(RefRole.generateUri(refRoleWithoutCourse)), refRoleGraphWithoutCourse).as[RefRole]

      expectedRefRole match {
        case Success(s) =>
          s shouldEqual refRoleWithoutCourse
        case Failure(e) =>
          fail(s"Unable to deserialise refRole graph: $e")
      }
    }
  }
}
