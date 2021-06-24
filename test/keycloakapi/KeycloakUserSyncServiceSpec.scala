package keycloakapi

import base.{AsyncSpec, LwmFakeApplication, TestBaseDefinition}
import models.{Employee, Lecturer, Student}
import org.scalatest.WordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.{JsError, JsSuccess}

import java.util.UUID

class KeycloakUserSyncServiceSpec
    extends WordSpec
    with TestBaseDefinition
    with GuiceOneAppPerSuite
    with LwmFakeApplication
    with AsyncSpec {

  val service = app.injector.instanceOf(classOf[KeycloakUserSyncService])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  "A KeycloakUserSyncService" should {
    "update an existing student by its keycloak representation" in {
      val enrollment = UUID.randomUUID()
      val stu = Student("lec", "sl1", "sf1", "se1", "sr1", enrollment)
      val k1 = KeycloakUser("kf1", "kl1", "ke1", "lec", Some("D1"), Some("kr1"))
      val Seq(res) =
        service.update(Seq(JsSuccess((stu, k1))))(_ => Some(enrollment))
      val user = res.right.value.asInstanceOf[Student]
      user.id shouldBe stu.id
      user.systemId shouldBe stu.systemId
      user.firstname shouldBe k1.firstname
      user.lastname shouldBe k1.lastname
      user.email shouldBe k1.email
      user.registrationId shouldBe k1.registrationId.value
      user.enrollment shouldBe enrollment
    }

    "update an existing employee by its keycloak representation" in {
      val emp = Employee("e1", "el1", "ef1", "ee1", UUID.randomUUID())
      val k1 = KeycloakUser("kf1", "kl1", "ke1", "e1", None, None)
      val Seq(res) = service.update(Seq(JsSuccess((emp, k1))))(_ => None)
      val user = res.right.value.asInstanceOf[Employee]
      user.id shouldBe emp.id
      user.systemId shouldBe emp.systemId
      user.firstname shouldBe k1.firstname
      user.lastname shouldBe k1.lastname
      user.email shouldBe k1.email
    }

    "update an existing lecturer by its keycloak representation" in {
      val lec = Lecturer("l1", "ll1", "lf1", "le1", UUID.randomUUID())
      val k1 = KeycloakUser("kf1", "kl1", "ke1", "l1", None, None)
      val Seq(res) = service.update(Seq(JsSuccess((lec, k1))))(_ => None)
      val user = res.right.value.asInstanceOf[Lecturer]
      user.id shouldBe lec.id
      user.systemId shouldBe lec.systemId
      user.firstname shouldBe k1.firstname
      user.lastname shouldBe k1.lastname
      user.email shouldBe k1.email
    }

    "not update an existing user if there is an id miss match" in {
      val s1 = Employee("e1", "el1", "ef1", "ee1", UUID.randomUUID())
      val k1 = KeycloakUser("kf1", "kl1", "ke1", "k1", None, None)
      val Seq(res) = service.update(Seq(JsSuccess((s1, k1))))(_ => None)
      res.left.value.startsWith("user miss match") shouldBe true
    }

    "not update an existing user if there is a parsing error" in {
      val Seq(res) = service.update(Seq(JsError("parsing error")))(_ => None)
      res.left.value.contains("parsing error") shouldBe true
    }

    "not update an existing student if the enrollment is unknown" in {
      val enrollment = UUID.randomUUID()
      val stu = Student("lec", "sl1", "sf1", "se1", "sr1", enrollment)
      val k1 = KeycloakUser("kf1", "kl1", "ke1", "lec", Some("D1"), Some("kr1"))
      val Seq(res) =
        service.update(Seq(JsSuccess((stu, k1))))(_ => None)
      res.left.value shouldBe "no degree found for D1"
    }

    "not update an existing student if enrollment or registrationId is missing" in {
      val enrollment = UUID.randomUUID()
      val stu = Student("lec", "sl1", "sf1", "se1", "sr1", enrollment)
      val k1 = KeycloakUser("kf1", "kl1", "ke1", "lec", None, None)
      val Seq(res) =
        service.update(Seq(JsSuccess((stu, k1))))(_ => Some(enrollment))
      res.left.value shouldBe "student must have a registration id and an enrollment"
    }
  }
}
