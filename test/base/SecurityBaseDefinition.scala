package base

import java.util.UUID

import models.Authority
import org.scalatest.mock.MockitoSugar
import play.api.ApplicationLoader.Context
import play.api.test.WithApplicationLoader
import play.api.{Application, ApplicationLoader}
import services.{GroupService, GroupServiceLike, RoleService, SessionHandlingService}
import utils.DefaultLwmApplication

trait FakeAuthority {

  val FakeCourse = UUID.randomUUID
  val FakeLabwork = UUID.randomUUID
  val FakeCourseUri = s"/courses/$FakeCourse"
  val FakeLabworkUri = s"labworks/$FakeLabwork"

  val FakeAdmin = UUID.randomUUID
  val FakeAdminAuth = Authority(FakeAdmin, UUID.randomUUID)

  val FakeRv = UUID.randomUUID
  val FakeRvAuth = Authority(FakeRv, UUID.randomUUID)

  val FakeMv = UUID.randomUUID
  val FakeMvAuth = Authority(FakeMv, UUID.randomUUID)

  val FakeMa = UUID.randomUUID
  val FakeMaAuth = Authority(FakeMa, UUID.randomUUID)

  val FakeHk = UUID.randomUUID
  val FakeHkAuth = Authority(FakeHk, UUID.randomUUID)

  val FakeEmployee = UUID.randomUUID
  val FakeEmployeeAuth =  Authority(FakeEmployee, UUID.randomUUID)

  val FakeStudent = UUID.randomUUID
  val FakeStudentAuth =  Authority(FakeStudent, UUID.randomUUID)
}

trait SecurityBaseDefinition extends FakeAuthority { self =>

  lazy val roleService = MockitoSugar.mock[RoleService]
  lazy val sessionService = MockitoSugar.mock[SessionHandlingService]
  lazy val groupService = MockitoSugar.mock[GroupService]

  class FakeApplication extends WithApplicationLoader(new ApplicationLoader {

    override def load(context: Context): Application = new DefaultLwmApplication(context) {

      override lazy val roleService: RoleService = self.roleService

      override lazy val sessionService: SessionHandlingService = self.sessionService

      override lazy val groupService: GroupServiceLike = self.groupService
    }.application
  })
}
