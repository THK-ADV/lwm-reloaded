package bind

import java.util.UUID

import base.SesameDbSpec
import models._
import org.joda.time.{LocalDate, LocalTime}
import org.w3.banana.PointedGraph

import scala.util.{Failure, Success}

class AnnotationBindingSpec extends SesameDbSpec {

  import bindings.{AnnotationDescriptor, dateTimeBinder, uuidBinder, uuidRefBinder}
  import ops._

  implicit val annotationBinder = AnnotationDescriptor.binder
  val annotation = Annotation(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, "message")
  val annotationGraph = URI(Annotation.generateUri(annotation)).a(lwm.Annotation)
    .--(lwm.student).->-(annotation.student)(ops, uuidRefBinder(User.splitter))
    .--(lwm.labwork).->-(annotation.labwork)(ops, uuidRefBinder(Labwork.splitter))
    .--(lwm.reportCardEntry).->-(annotation.reportCardEntry)(ops, uuidRefBinder(ReportCardEntry.splitter))
    .--(lwm.message).->-(annotation.message)
    .--(lwm.timestamp).->-(annotation.timestamp)
    .--(lwm.invalidated).->-(annotation.invalidated)
    .--(lwm.id).->-(annotation.id).graph

  "A AnnotationBindingSpec " should {

    "return a RDF graph representation of an annotation" in {
      val graph = annotation.toPG.graph

      graph isIsomorphicWith annotationGraph shouldBe true
    }

    "return a annotation based on an RDF graph representation" in {
      val expectedAnnotation = PointedGraph[Rdf](URI(Annotation.generateUri(annotation)), annotationGraph).as[Annotation]

      expectedAnnotation match {
        case Success(s) =>
          s shouldEqual annotation
        case Failure(e) =>
          fail(s"Unable to deserialise annotation graph: $e")
      }
    }

    "return an atomic annotation based on an RDF graph representation" in {
      import bindings.{AnnotationAtomDescriptor, AnnotationDescriptor, LabworkDescriptor, ReportCardEntryDescriptor, StudentDescriptor}

      val student = SesameStudent("systemid", "lastname", "firstname", "email", "registrationId", UUID.randomUUID())
      val labwork = Labwork("label", "description", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
      val reportCardEntry = ReportCardEntry(student.id, labwork.id, "label", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID(), Set.empty)
      val annot = Annotation(student.id, labwork.id, reportCardEntry.id, "message")
      val annotAtom = AnnotationAtom(student, labwork, reportCardEntry, annot.message, annot.timestamp, annot.invalidated, annot.id)

      repo.add(student)
      repo.add(labwork)
      repo.add(reportCardEntry)
      repo.add(annot)

      repo.get[AnnotationAtom](Annotation.generateUri(annot)) match {
        case Success(Some(dannot)) =>
          dannot.labwork shouldEqual annotAtom.labwork
          dannot.message shouldEqual annotAtom.message
          dannot.reportCardEntry shouldEqual annotAtom.reportCardEntry
          dannot.student shouldEqual annotAtom.student
          dannot.timestamp isEqual annotAtom.timestamp shouldBe true
          dannot.id shouldEqual annotAtom.id

        case Success(None) =>
          fail("There should be one annotation")
        case Failure(e) =>
          fail(s"Unable to deserialise annotation atom: $e")
      }
    }
  }
}
