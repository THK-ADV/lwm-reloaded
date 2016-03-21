package controllers.security

import java.util.UUID

import models.security.Authority
import org.scalatest.mock.MockitoSugar
import play.api.{Application, ApplicationLoader}
import play.api.ApplicationLoader.Context
import play.api.test.WithApplicationLoader
import services.{RoleService, SessionHandlingService}
import utils.DefaultLwmApplication

trait FakeAuthority {

  val FakeCourse = UUID.randomUUID()
  val FakeLabwork = UUID.randomUUID()
  val FakeCourseUri = s"/courses/$FakeCourse"
  val FakeLabworkUri = s"labworks/$FakeLabwork"

  val FakeAdmin = UUID.randomUUID()
  val FakeAdminAuth = Authority(FakeAdmin, Set(UUID.randomUUID()))

  val FakeRv = UUID.randomUUID()
  val FakeRvAuth = Authority(FakeRv, Set(UUID.randomUUID()))

  val FakeMv = UUID.randomUUID()
  val FakeMvAuth = Authority(FakeMv, Set(UUID.randomUUID()))

  val FakeMa = UUID.randomUUID()
  val FakeMaAuth = Authority(FakeMa, Set(UUID.randomUUID()))

  val FakeHk = UUID.randomUUID()
  val FakeHkAuth = Authority(FakeHk, Set(UUID.randomUUID()))

  val FakeEmployee = UUID.randomUUID()
  val FakeEmployeeAuth =  Authority(FakeEmployee, Set(UUID.randomUUID()))

  val FakeStudent = UUID.randomUUID()
  val FakeStudentAuth =  Authority(FakeStudent, Set(UUID.randomUUID()))
}

trait SecurityBaseDefinition extends FakeAuthority { self =>

  val roleService = MockitoSugar.mock[RoleService]
  val sessionService = MockitoSugar.mock[SessionHandlingService]

  class FakeApplication extends WithApplicationLoader(new ApplicationLoader {

    override def load(context: Context): Application = new DefaultLwmApplication(context) {

      override lazy val roleService: RoleService = self.roleService

      override val sessionService: SessionHandlingService = self.sessionService
    }.application
  })
}
