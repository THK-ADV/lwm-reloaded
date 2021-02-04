package dao

import java.util.UUID
import base.PostgresDbSpec
import database.RoleDb
import database.helper.LdapUserStatus._
import models.{Authority, Role}
import play.api.inject.guice.GuiceableModule
import security.LWMRole
import security.LWMRole._
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Future

class RoleDaoSpec extends PostgresDbSpec {

  import scala.concurrent.ExecutionContext.Implicits.global

  val dao = app.injector.instanceOf(classOf[RoleDao])
  val roles = LWMRole.all.map(r => RoleDb(r.label))

  def studentRole = roles.find(_.label == StudentRole.label).get

  def employeeRole = roles.find(_.label == EmployeeRole.label).get

  def adminRole = roles.find(_.label == Admin.label).get

  def courseEmployeeRole = roles.find(_.label == CourseEmployee.label).get

  def courseStudentRole = roles.find(_.label == CourseAssistant.label).get

  "A RoleDaoSpec" should {

    "return role which matches user status" in {
      runAsyncSequence(
        dao.byUserStatusQuery(StudentStatus) map (_.value shouldBe studentRole),
        dao.byUserStatusQuery(EmployeeStatus) map (_.value shouldBe employeeRole),
        dao.byUserStatusQuery(LecturerStatus) map (_.value shouldBe employeeRole)
      )
    }

    "return role which matches role label" in {
      runAsyncSequence(
        dao.byRoleLabelQuery(StudentRole.label) map (_.value shouldBe studentRole),
        dao.byRoleLabelQuery(EmployeeRole.label) map (_.value shouldBe employeeRole),
        dao.byRoleLabelQuery(Admin.label) map (_.value shouldBe adminRole)
      )
    }

    "not return role when not found" in {
      runAsyncSequence(
        dao.byRoleLabelQuery(God.label) map (_ shouldBe None),
        dao.byRoleLabelQuery("other") map (_ shouldBe None)
      )
    }

    "always pass authority validation if the user is an admin" in {
      val auths = List(Authority(UUID.randomUUID, adminRole.id))

      val results = Future.sequence(LWMRole.all.map(role => dao.isAuthorized(Some(UUID.randomUUID), List(role))(auths)))
      async(results)(_.reduce(_ && _) shouldBe true)
    }

    "always deny authority validation if god is requested" in {
      val user = UUID.randomUUID
      val auths = roles.map(r => Authority(user, r.id))

      async(dao.isAuthorized(None, List(God))(auths))(_ shouldBe false)
    }

    "check authority validation in different cases" in {
      val course1 = UUID.randomUUID
      val course2 = UUID.randomUUID
      val auths = List(
        Authority(UUID.randomUUID, employeeRole.id),
        Authority(UUID.randomUUID, courseEmployeeRole.id, Some(course1)),
        Authority(UUID.randomUUID, courseStudentRole.id, Some(course2))
      )

      async(dao.isAuthorized(None, List(EmployeeRole, StudentRole))(auths))(_ shouldBe true)
      async(dao.isAuthorized(Some(course1), List(CourseEmployee))(auths))(_ shouldBe true)
      async(dao.isAuthorized(Some(course1), List(CourseEmployee, CourseAssistant))(auths))(_ shouldBe true)
      async(dao.isAuthorized(Some(course1), List(EmployeeRole, CourseEmployee))(auths))(_ shouldBe true)
      async(dao.isAuthorized(Some(course2), List(CourseEmployee, CourseAssistant))(auths))(_ shouldBe true)
      async(dao.isAuthorized(Some(course2), List(EmployeeRole, CourseEmployee))(auths))(_ shouldBe false)
      async(dao.isAuthorized(Some(UUID.randomUUID), List(CourseEmployee, CourseAssistant))(auths))(_ shouldBe false)
      async(dao.isAuthorized(None, List(StudentRole))(auths))(_ shouldBe false)
      async(dao.isAuthorized(None, List(CourseManager))(auths))(_ shouldBe false)
      async(dao.isAuthorized(None, List(Admin))(auths))(_ shouldBe false)
      async(dao.isAuthorized(Some(course1), List(Admin))(auths))(_ shouldBe false)
      async(dao.isAuthorized(Some(UUID.randomUUID), List.empty)(auths))(_ shouldBe false)
      async(dao.isAuthorized(None, List.empty)(auths))(_ shouldBe false)
      async(dao.isAuthorized(None, List(StudentRole))(Seq.empty))(_ shouldBe false)
    }
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    async(dao.createMany(roles))(_ => Unit)
  }

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()
}
