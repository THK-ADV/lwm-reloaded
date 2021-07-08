package keycloakapi

import base.{AsyncSpec, FileSpec, LwmFakeApplication, TestBaseDefinition}
import models.{Employee, Student, User}
import org.scalatest.WordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.{JsError, JsResult, JsSuccess, Json}

import java.util.UUID

class KeycloakApiServiceSpec
    extends WordSpec
    with TestBaseDefinition
    with GuiceOneAppPerSuite
    with LwmFakeApplication
    with AsyncSpec
    with FileSpec {

  val service = app.injector.instanceOf(classOf[KeycloakApiService])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  "A KeycloakApiService" should {

    "parse a bearer token" in {
      val token = Json.parse(readTestResourceFile("bearer_token.json"))
      async(service.parseBearerToken(token))(assertBearerToken)
    }

    "parse a keycloak student" in {
      val studentJson =
        Json.parse(readTestResourceFile("keycloak_student.json"))
      val res = service.parseKeycloakUser(studentJson)
      val users = getJson(res)
      users.size shouldBe 1

      val student = users.head
      student.systemId shouldBe "inf001"
      student.email shouldBe "foo@foo.com"
      student.lastname shouldBe "Avram"
      student.firstname shouldBe "Robert"
      student.degreeAbbrev.value shouldBe "MI"
      student.registrationId.value shouldBe "12345678"
    }

    "parse a keycloak employee" in {
      val employeeJson =
        Json.parse(readTestResourceFile("keycloak_employee.json"))
      val res = service.parseKeycloakUser(employeeJson)
      val users = getJson(res)
      users.size shouldBe 1

      val employee = users.head
      employee.systemId shouldBe "lwmadmin"
      employee.email shouldBe "uwe.muesse@th-koeln.de"
      employee.lastname shouldBe "Admin"
      employee.firstname shouldBe "LWM2"
      employee.degreeAbbrev shouldBe None
      employee.registrationId shouldBe None
    }

    "authenticate to admin-cli" in {
      async(service.authenticate)(assertBearerToken)
    }

    "fetch a student" in {
      val user: User =
        Student(
          "ravram",
          "fake",
          "fake",
          "fake",
          "fake",
          "fake",
          UUID.randomUUID()
        )

      async(service.fetchUser(user)(_.systemId)) { case (u, res) =>
        val student = getJson(res)

        u shouldBe user
        student.systemId shouldBe "inf001"
        student.email shouldBe "foo@foo.com"
        student.lastname shouldBe "Avram"
        student.firstname shouldBe "Robert"
        student.degreeAbbrev.value shouldBe "MI"
        student.registrationId.value shouldBe "12345678"
      }
    }

    "fetch an employee" in {
      val user: User =
        Employee("lwmadmin", "fake", "fake", "fake", "fake", UUID.randomUUID())

      async(service.fetchUser(user)(_.systemId)) { case (u, res) =>
        val employee = getJson(res)

        u shouldBe user
        employee.systemId shouldBe "lwmadmin"
        employee.email shouldBe "uwe.muesse@th-koeln.de"
        employee.lastname shouldBe "Admin"
        employee.firstname shouldBe "LWM2"
        employee.degreeAbbrev shouldBe None
        employee.registrationId shouldBe None
      }
    }
  }

  def getJson[A](json: JsResult[A]): A = json match {
    case JsSuccess(value, _) => value
    case JsError(errors)     => fail(s"json parsing failed with error: $errors")
  }

  def assertBearerToken(bearerToken: String) = {
    val Array(bearer, token) = bearerToken.split(" ")
    bearer shouldBe "bearer"
    token.nonEmpty shouldBe true
  }
}
