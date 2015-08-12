package bind.security

import base.SesameDbSpec
import models.Course
import models.security.{Permission, Role, RefRole}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.Namespace
import store.bind.Bindings

import scala.util.{Failure, Success}

class RefRoleBindingSpec extends SesameDbSpec {
  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings.RefRoleBinding._
  import bindings.uuidBinder
  import bindings.roleBinder

  val refRoleWithCourse = RefRole(
    Some(Course.randomUUID),
    Role("role", Set(Permission("perm"))),
    RefRole.randomUUID
  )

  val refRoleWithoutCourse = RefRole(
    None,
    Role("role", Set(Permission("perm"))),
    RefRole.randomUUID
  )

  val refRoleGraphWithCourse = (
    URI(RefRole.generateUri(refRoleWithCourse)).a(lwm.RefRole)
      -- lwm.module ->- refRoleWithCourse.module
      -- lwm.role->- refRoleWithCourse.role
      -- lwm.id->- refRoleWithCourse.id
    ).graph

  val refRoleGraphWithoutCourse = (
    URI(RefRole.generateUri(refRoleWithoutCourse)).a(lwm.RefRole)
      -- lwm.module ->- refRoleWithoutCourse.module
      -- lwm.role->- refRoleWithoutCourse.role
      -- lwm.id->- refRoleWithoutCourse.id
    ).graph

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
