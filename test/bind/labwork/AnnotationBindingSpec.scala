package bind.labwork

import java.util.UUID

import base.SesameDbSpec
import models.labwork.{ReportCardEntry, Annotation, Labwork}
import models.users.User
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.{Failure, Success}

class AnnotationBindingSpec extends SesameDbSpec {

  import ops._

  val bindings = Bindings[Sesame](namespace)
  import bindings.AnnotationBinding.annotationBinding
  import bindings.{uuidBinder, uuidRefBinder, jodaDateTimeBinder}

  val annotation = Annotation(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, "message")
  val annotationGraph = URI(Annotation.generateUri(annotation)).a(lwm.Annotation)
    .--(lwm.student).->-(annotation.student)(ops, uuidRefBinder(User.splitter))
    .--(lwm.labwork).->-(annotation.labwork)(ops, uuidRefBinder(Labwork.splitter))
    .--(lwm.reportCardEntry).->-(annotation.reportCardEntry)(ops, uuidRefBinder(ReportCardEntry.splitter))
    .--(lwm.message).->-(annotation.message)
    .--(lwm.timestamp).->-(annotation.timestamp)
    .--(lwm.id).->-(annotation.id).graph

  "A AnnotationBindingSpec " should {

    "return a RDF graph representation of a annotation" in {
      val graph = annotation.toPG.graph

      graph isIsomorphicWith annotationGraph shouldBe true
    }

    "return a annotation based on a RDF graph representation" in {
      val expectedAnnotation = PointedGraph[Rdf](URI(Annotation.generateUri(annotation)), annotationGraph).as[Annotation]

      expectedAnnotation match {
        case Success(s) =>
          s shouldEqual annotation
        case Failure(e) =>
          fail(s"Unable to deserialise annotation graph: $e")
      }
    }
  }
}
