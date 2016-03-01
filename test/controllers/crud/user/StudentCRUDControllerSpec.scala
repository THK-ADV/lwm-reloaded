package controllers.crud.user

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models.Degree
import models.users.{StudentAtom, Student, StudentProtocol}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import store.SesameRepository
import store.sparql.{NoneClause, select, Clause}
import utils.LwmMimeType

import scala.util.{Failure, Success}

class StudentCRUDControllerSpec extends AbstractCRUDControllerSpec[StudentProtocol, Student] {

  val degreeToPass = Degree("label to pass", "abbrev to pass", Degree.randomUUID)
  val degreeToFail = Degree("label to fail", "abbrev to fail", Degree.randomUUID)

  override val entityToPass: Student = Student(
    "system id to pass",
    "surname to pass",
    "forename to pass",
    "email to pass",
    "registration id to pass",
    degreeToPass.id,
    Student.randomUUID
  )

  override def entityTypeName: String = "student"

  override val controller: AbstractCRUDController[StudentProtocol, Student] = new StudentCRUDController(repository, namespace, roleService) {

    override protected def fromInput(input: StudentProtocol, id: Option[UUID]): Student = entityToPass
  }

  override val entityToFail: Student = Student(
    "system id to fail",
    "surname to fail",
    "forename to fail",
    "email to fail",
    "registration id to fail",
    degreeToFail.id,
    Student.randomUUID
  )

  override implicit val jsonWrites: Writes[Student] = Student.writes

  override val mimeType: LwmMimeType = LwmMimeType.studentV1Json

  override val inputJson: JsValue = Json.obj(
    "systemId" -> entityToPass.systemId,
    "lastname" -> entityToPass.lastname,
    "firstname" -> entityToPass.firstname,
    "email" -> entityToPass.email,
    "registrationId" -> entityToPass.registrationId,
    "enrollment" -> entityToPass.enrollment
  )

  import ops._
  import bindings.StudentBinding._
  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val updateJson: JsValue = Json.obj(
    "systemId" -> s"${entityToPass.systemId} updated",
    "lastname" -> s"${entityToPass.lastname} updated",
    "firstname" -> s"${entityToPass.firstname} updated",
    "email" -> s"${entityToPass.email} updated",
    "registrationId" -> s"${entityToPass.registrationId} updated",
    "enrollment" -> entityToPass.enrollment
  )

  val atomizedEntityToPass = StudentAtom(
    entityToPass.systemId,
    entityToPass.lastname,
    entityToPass.firstname,
    entityToPass.email,
    entityToPass.registrationId,
    degreeToPass,
    entityToPass.id
  )

  val atomizedEntityToFail = StudentAtom(
    entityToFail.systemId,
    entityToFail.lastname,
    entityToFail.firstname,
    entityToFail.email,
    entityToFail.registrationId,
    degreeToFail,
    entityToFail.id
  )

  "A StudentCRUDControllerSpec also " should {

    s"successfully get a single $entityTypeName atomized" in {
      import Student.atomicWrites

      doReturn(Success(Some(entityToPass))).
      doReturn(Success(Some(degreeToPass))).
      when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(atomizedEntityToPass)
    }

    s"not get a single $entityTypeName atomized when degree is not found" in {
      doReturn(Success(Some(entityToPass))).
      doReturn(Success(None)).
      when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    s"not get a single $entityTypeName atomized when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"

      doReturn(Success(Some(entityToPass))).
      doReturn(Failure(new Exception(errorMessage))).
      when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    s"successfully get all ${fgrammar(entityTypeName)} atomized" in {
      import Student.atomicWrites

      val students = Set(entityToPass, entityToFail)
      val degrees = Vector(degreeToPass, degreeToFail)

      when(repository.get[Student](anyObject(), anyObject())).thenReturn(Success(students))
      when(repository.getMany[Degree](anyObject())(anyObject())).thenReturn(Success(degrees))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s"
      )
      val result = controller.allAtomic()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(atomizedEntityToPass, atomizedEntityToFail))
    }

    s"not get all ${fgrammar(entityTypeName)} atomized when there is an exception" in {
      val students = Set(entityToPass, entityToFail)
      val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"

      when(repository.get[Student](anyObject(), anyObject())).thenReturn(Success(students))
      when(repository.getMany[Degree](anyObject())(anyObject())).thenReturn(Failure(new Exception(errorMessage)))

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s"
      )
      val result = controller.allAtomic()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }
  }
}

