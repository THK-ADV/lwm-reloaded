package controllers.crud.labwork

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models.labwork._
import models.users.Student
import org.joda.time.{LocalTime, LocalDate}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Writes, JsValue}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import utils.LwmMimeType
import org.mockito.Matchers._
import org.mockito.Mockito._

import scala.util.{Failure, Success}

class AnnotationCRUDControllerSpec extends AbstractCRUDControllerSpec[AnnotationProtocol, Annotation, AnnotationAtom] {

  val studentToPass = Student("systemId to pass", "last name to pass", "first name to pass", "email to pass", "regId to pass", UUID.randomUUID())
  val studentToFail = Student("systemId to fail", "last name to fail", "first name to fail", "email to fail", "regId to fail", UUID.randomUUID())

  val labworkToPass = Labwork("label to pass", "desc to pass", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
  val labworkToFail = Labwork("label to fail", "desc to fail", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())

  val entryToPass = ReportCardEntry(studentToPass.id, labworkToPass.id, "label to pass", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, Set.empty[ReportCardEntryType])
  val entryToFail = ReportCardEntry(studentToFail.id, labworkToFail.id, "label to fail", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, Set.empty[ReportCardEntryType])

  override val entityToFail: Annotation = Annotation(studentToFail.id, labworkToFail.id, entryToFail.id, "message to fail")

  override val entityToPass: Annotation = Annotation(studentToPass.id, labworkToPass.id, entryToPass.id, "message to pass")

  import ops._
  import bindings.AnnotationDescriptor

  implicit val annotationBinder = AnnotationDescriptor.binder

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override def entityTypeName: String = "annotation"

  override val controller: AnnotationCRUDController = new AnnotationCRUDController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: AnnotationProtocol, existing: Option[Annotation]): Annotation = entityToPass

    override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override implicit val jsonWrites: Writes[Annotation] = Annotation.writes

  override implicit def jsonWritesAtom: Writes[AnnotationAtom] = Annotation.writesAtom

  override val mimeType: LwmMimeType = LwmMimeType.annotationV1Json

  override val updateJson: JsValue = Json.obj(
    "student" -> entityToPass.student,
    "labwork" -> entityToPass.labwork,
    "reportCardEntry" -> entityToPass.reportCardEntry,
    "message" -> (entityToPass.message + " updated"),
    "timestamp" -> entityToPass.timestamp
  )

  override val inputJson: JsValue = Json.obj(
    "student" -> entityToPass.student,
    "labwork" -> entityToPass.labwork,
    "reportCardEntry" -> entityToPass.reportCardEntry,
    "message" -> entityToPass.message,
    "timestamp" -> entityToPass.timestamp
  )

  override val atomizedEntityToPass = AnnotationAtom(
    studentToPass,
    labworkToPass,
    entryToPass,
    entityToPass.message,
    entityToPass.timestamp,
    entityToPass.id
  )

  override val atomizedEntityToFail = AnnotationAtom(
    studentToFail,
    labworkToFail,
    entryToFail,
    entityToFail.message,
    entityToFail.timestamp,
    entityToFail.id
  )


  "A AnnotationCRUDControllerSpec also " should {

    "return all annotations for a given labwork" in {
      val labwork = UUID.randomUUID
      val first = Annotation(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, "message 1")
      val second = Annotation(UUID.randomUUID, labwork, UUID.randomUUID, "message 2")
      val third = Annotation(UUID.randomUUID, labwork, UUID.randomUUID, "message 3")
      val fourth = Annotation(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, "message 4")

      when(repository.getAll[Annotation](anyObject())).thenReturn(Success(Set(first, second, third, fourth)))


      val request = FakeRequest(
        GET,
        s"/annotations?${AnnotationCRUDController.labworkAttribute}=$labwork"
      )
      val result = controller.all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(second, third))
    }

    "return all annotations for a given student" in {
      val student = UUID.randomUUID
      val first = Annotation(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, "message 1")
      val second = Annotation(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, "message 2")
      val third = Annotation(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, "message 3")
      val fourth = Annotation(student, UUID.randomUUID, UUID.randomUUID, "message 4")

      when(repository.getAll[Annotation](anyObject())).thenReturn(Success(Set(first, second, third, fourth)))


      val request = FakeRequest(
        GET,
        s"/annotations?${AnnotationCRUDController.studentAttribute}=$student"
      )
      val result = controller.all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(fourth))
    }

    "return all annotations for a given report card entry" in {
      val reportCardEntry = UUID.randomUUID
      val first = Annotation(UUID.randomUUID, UUID.randomUUID, reportCardEntry, "message 1")
      val second = Annotation(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, "message 2")
      val third = Annotation(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, "message 3")
      val fourth = Annotation(UUID.randomUUID, UUID.randomUUID, reportCardEntry, "message 4")

      when(repository.getAll[Annotation](anyObject())).thenReturn(Success(Set(first, second, third, fourth)))


      val request = FakeRequest(
        GET,
        s"/annotations?${AnnotationCRUDController.reportCardEntryAttribute}=$reportCardEntry"
      )
      val result = controller.all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(first, fourth))
    }

    "return all annotations for a given student in a certain labwork" in {
      val student = UUID.randomUUID
      val labwork = UUID.randomUUID
      val first = Annotation(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, "message 1")
      val second = Annotation(student, UUID.randomUUID, UUID.randomUUID, "message 2")
      val third = Annotation(UUID.randomUUID, labwork, UUID.randomUUID, "message 3")
      val fourth = Annotation(student, labwork, UUID.randomUUID, "message 4")

      when(repository.getAll[Annotation](anyObject())).thenReturn(Success(Set(first, second, third, fourth)))


      val request = FakeRequest(
        GET,
        s"/annotations?${AnnotationCRUDController.studentAttribute}=$student&${AnnotationCRUDController.labworkAttribute}=$labwork"
      )
      val result = controller.all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(fourth))
    }
  }
}
