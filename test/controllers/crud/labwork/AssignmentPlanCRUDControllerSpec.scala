package controllers.crud.labwork

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models.labwork._
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import utils.LwmMimeType

import scala.util.{Failure, Success}

class AssignmentPlanCRUDControllerSpec extends AbstractCRUDControllerSpec[AssignmentPlanProtocol, AssignmentPlan] {

  override def entityTypeName: String = "assignmentPlan"

  override val controller: AbstractCRUDController[AssignmentPlanProtocol, AssignmentPlan] = new AssignmentPlanCRUDController(repository, sessionService, namespace, roleService) {

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def fromInput(input: AssignmentPlanProtocol, existing: Option[AssignmentPlan]): AssignmentPlan = entityToPass
  }

  def entries(s: String): Set[AssignmentEntry] = (0 until 5).map( n =>
    AssignmentEntry(n, s"${n.toString} to $s", AssignmentEntryType.all)
  ).toSet

  val labworkToPass = Labwork("label to pass", "desc to pass", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
  val labworkToFail = Labwork("label to fail", "desc to fail", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())

  override val entityToFail: AssignmentPlan = AssignmentPlan(labworkToFail.id, 5, 5, entries("fail"))

  override val entityToPass: AssignmentPlan = AssignmentPlan(labworkToPass.id, 5, 5, entries("pass"))

  override implicit val jsonWrites: Writes[AssignmentPlan] = AssignmentPlan.writes

  override val mimeType: LwmMimeType = LwmMimeType.assignmentPlanV1Json

  import ops._
  import bindings.AssignmentPlanDescriptor

  implicit val assignmentPlanBinder = AssignmentPlanDescriptor.binder
  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val updateJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "attendance" -> entityToPass.attendance,
    "mandatory" -> entityToPass.mandatory,
    "entries" -> entityToPass.entries.drop(2)
  )

  override val inputJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "attendance" -> entityToPass.attendance,
    "mandatory" -> entityToPass.mandatory,
    "entries" -> entityToPass.entries
  )

  "An AssignmentPlanCRUDControllerSpec also " should {

    "return all assignment plans for a given labwork" in {
      val labwork = UUID.randomUUID
      val ap1 = AssignmentPlan(labwork, 0, 0, entries("ap1"))
      val ap2 = AssignmentPlan(UUID.randomUUID, 0, 0, entries("ap2"))
      val ap3 = AssignmentPlan(labwork, 0, 0, entries("ap3"))
      val ap4 = AssignmentPlan(UUID.randomUUID, 0, 0, entries("ap4"))

      when(repository.getAll[AssignmentPlan](anyObject())).thenReturn(Success(Set(ap1, ap2, ap3, ap4)))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${AssignmentPlanCRUDController.labworkAttribute}=$labwork"
      )
      val result = controller.all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(ap1, ap3))
    }

    "not return all assignment plans for a given labwork when there is an exception" in {
      val labwork = UUID.randomUUID
      val errorMessage = "Oops, something went wrong"

      when(repository.getAll[AssignmentPlan](anyObject())).thenReturn(Failure(new Throwable(errorMessage)))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${AssignmentPlanCRUDController.labworkAttribute}=$labwork"
      )
      val result = controller.all()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    "return all assignment plans for a given course" in {
      val course = UUID.randomUUID
      val lab1 = Labwork("", "", UUID.randomUUID, course, UUID.randomUUID)
      val lab2 = Labwork("", "", UUID.randomUUID, course, UUID.randomUUID)
      val ap1 = AssignmentPlan(lab1.id, 0, 0, entries("ap1"))
      val ap2 = AssignmentPlan(UUID.randomUUID, 0, 0, entries("ap2"))
      val ap3 = AssignmentPlan(UUID.randomUUID, 0, 0, entries("ap3"))
      val ap4 = AssignmentPlan(lab1.id, 0, 0, entries("ap4"))
      val ap5 = AssignmentPlan(lab2.id, 0, 0, entries("ap5"))
      val ap6 = AssignmentPlan(UUID.randomUUID, 0, 0, entries("ap6"))
      val ap7 = AssignmentPlan(lab2.id, 0, 0, entries("ap7"))
      val ap8 = AssignmentPlan(lab1.id, 0, 0, entries("ap8"))

      when(repository.getAll[AssignmentPlan](anyObject())).thenReturn(Success(Set(
        ap1, ap2, ap3, ap4, ap5, ap6, ap7, ap8
      )))
      when(repository.getMany[Labwork](anyObject())(anyObject())).thenReturn(Success(Set(lab1, lab2)))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${AssignmentPlanCRUDController.courseAttribute}=$course"
      )
      val result = controller.all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(ap1, ap4, ap5, ap7, ap8))
    }

    "return an empty json when there are no assignment plans for a given course" in {
      val course = UUID.randomUUID
      val lab1 = Labwork("", "", UUID.randomUUID, course, UUID.randomUUID)
      val lab2 = Labwork("", "", UUID.randomUUID, course, UUID.randomUUID)
      val aps = (0 until 1).map(i => AssignmentPlan(UUID.randomUUID, 0, 0, entries(i.toString))).toSet

      when(repository.getAll[AssignmentPlan](anyObject())).thenReturn(Success(aps))
      when(repository.getMany[Labwork](anyObject())(anyObject())).thenReturn(Success(Set(lab1, lab2)))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?${AssignmentPlanCRUDController.courseAttribute}=$course"
      )
      val result = controller.all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set.empty[AssignmentPlan])
    }

    "not return assignment plans when there is an invalid query attribute" in {
      val aps = (0 until 1).map(i => AssignmentPlan(UUID.randomUUID, 0, 0, entries(i.toString))).toSet

      when(repository.getAll[AssignmentPlan](anyObject())).thenReturn(Success(aps))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName.toLowerCase}s?invalidAttribute=${UUID.randomUUID}"
      )
      val result = controller.all()(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "Unknown attribute"
      )
    }
  }
}
