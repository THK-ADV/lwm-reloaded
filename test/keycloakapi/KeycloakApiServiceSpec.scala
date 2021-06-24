package keycloakapi

import base.{AsyncSpec, LwmFakeApplication, TestBaseDefinition}
import models.{Employee, Student, User}
import org.scalatest.WordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.{JsError, JsResult, JsSuccess}

import java.util.UUID

class KeycloakApiServiceSpec
    extends WordSpec
    with TestBaseDefinition
    with GuiceOneAppPerSuite
    with LwmFakeApplication
    with AsyncSpec {

  val service = app.injector.instanceOf(classOf[KeycloakApiService])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  "A KeycloakApiService" should {
    "authenticate to admin-cli" in {
      async(service.authenticate) { bearerToken =>
        val Array(bearer, token) = bearerToken.split(" ")
        bearer shouldBe "Bearer"
        token.nonEmpty shouldBe true
      }
    }

    "fetch a student" in {
      val user: User =
        Student("ravram", "fake", "fake", "fake", "fake", UUID.randomUUID())

      async(service.fetchUsers(List(user))(_.systemId)) { res =>
        res.size shouldBe 1
        val (u, student) = getJson(res.head)

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
        Employee("lwmadmin", "fake", "fake", "fake", UUID.randomUUID())

      async(service.fetchUsers(List(user))(_.systemId)) { res =>
        res.size shouldBe 1
        val (u, employee) = getJson(res.head)

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
}
