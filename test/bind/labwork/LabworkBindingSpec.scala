package bind.labwork

import base.SesameDbSpec
import models.labwork.Labwork
import models.semester.Semester
import models.{Course, Degree}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.{Failure, Success}

class LabworkBindingSpec extends SesameDbSpec {

  import ops._

  val bindings = Bindings[Sesame](namespace)

  import bindings.LabworkBinding.labworkBinder
  import bindings.{uuidBinder, uuidRefBinder}

  val labwork = Labwork("AP Praktikum", "AP Praktikum", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
  val labworkGraph = URI(Labwork.generateUri(labwork)).a(lwm.Labwork)
    .--(lwm.label).->-(labwork.label)
    .--(lwm.description).->-(labwork.description)
    .--(lwm.semester).->-(labwork.semester)(ops, uuidRefBinder(Semester.splitter))
    .--(lwm.course).->-(labwork.course)(ops, uuidRefBinder(Course.splitter))
    .--(lwm.degree).->-(labwork.degree)(ops, uuidRefBinder(Degree.splitter))
    .--(lwm.subscribable).->-(labwork.subscribable)
    .--(lwm.id).->-(labwork.id).graph

  "A LabworkBinding" should {
    "return a RDF graph representation of a labwork" in {
      val graph = labwork.toPG.graph

      graph isIsomorphicWith labworkGraph shouldBe true
    }

    "return a labwork based on a RDF representation" in {
      val expectedLabwork = PointedGraph[Rdf](URI(Labwork.generateUri(labwork)), labworkGraph).as[Labwork]

      expectedLabwork match {
        case Success(s) =>
          s shouldEqual labwork
        case Failure(e) =>
          fail(s"Unable to deserialise labwork graph: $e")
      }
    }
  }
}
