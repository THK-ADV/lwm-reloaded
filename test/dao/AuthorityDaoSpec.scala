package dao

import java.util.UUID

import dao.helper.TableFilter
import database._
import database.helper.LdapUserStatus._
import models._
import org.joda.time.DateTime
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write

class AuthorityDaoSpec extends AbstractDaoSpec[AuthorityTable, AuthorityDb, AuthorityLike] {

  import AbstractDaoSpec._
  import slick.jdbc.PostgresProfile.api._
  import utils.date.DateTimeOps.DateTimeConverter

  import scala.concurrent.ExecutionContext.Implicits.global

  private def courseManager: RoleDb = roles.find(_.label == Role.CourseManager.label).get

  private def employeeRole: RoleDb = roles.find(_.label == Role.EmployeeRole.label).get

  private def studentRole: RoleDb = roles.find(_.label == Role.StudentRole.label).get

  private def adminRole: RoleDb = roles.find(_.label == Role.Admin.label).get

  "A AuthorityDaoSpec" should {

    "create a associated basic authority for a given user" in {
      val employee = UserDb("id", "last", "first", "mail", EmployeeStatus, None, None)
      val lecturer = UserDb("id", "last", "first", "mail", LecturerStatus, None, None)
      val student = UserDb("id", "last", "first", "mail", StudentStatus, None, None)

      runAsyncSequence(
        TableQuery[UserTable].forceInsertAll(List(employee, lecturer, student)),
        dao.createBasicAuthorityFor(employee) map { auth =>
          auth.user shouldBe employee.id
          auth.role shouldBe roles.find(_.label == employeeRole.label).get.id
          auth.course shouldBe empty
        },
        dao.createBasicAuthorityFor(lecturer) map { auth =>
          auth.user shouldBe lecturer.id
          auth.role shouldBe roles.find(_.label == employeeRole.label).get.id
          auth.course shouldBe empty
        },
        dao.createBasicAuthorityFor(student) map { auth =>
          auth.user shouldBe student.id
          auth.role shouldBe roles.find(_.label == studentRole.label).get.id
          auth.course shouldBe empty
        }
      )
    }

    "not create a associated basic authority for a given user if he already has one" in {
      val student = UserDb("system id", "last", "first", "mail", StudentStatus, None, None)

      runAsyncSequence(
        TableQuery[UserTable].forceInsert(student),
        dao.createBasicAuthorityFor(student),
        dao.createBasicAuthorityFor(student).failed.map { t =>
          t.getMessage.containsSlice(student.id.toString) shouldBe true
        }
      )
    }

    "delete authorities as long as the are not associated with a basic role" in {
      val student = UserDb(UUID.randomUUID.toString, "last", "first", "mail", StudentStatus, None, None)
      val otherUser = UserDb(UUID.randomUUID.toString, "last", "first", "mail", EmployeeStatus, None, None)
      val nonBasicRoles = roles.filter(r => r.label == Role.CourseAssistant.label || r.label == Role.CourseEmployee.label)
      val auths = courses.take(2).zip(nonBasicRoles).map {
        case (course, role) => AuthorityDb(student.id, role.id, Some(course.id))
      }

      runAsyncSequence(
        TableQuery[UserTable].forceInsertAll(List(otherUser, student)),
        dao.createBasicAuthorityFor(student),
        dao.tableQuery.forceInsertAll(auths)
      )

      async(dao.deleteAuthorityIfNotBasic(auths.head.id))(a => nonBasicRoles.map(_.id).contains(a.role) shouldBe true)
      async(dao.deleteAuthorityIfNotBasic(auths.last.id))(a => nonBasicRoles.map(_.id).contains(a.role) shouldBe true)

      async(dao.get(List(TableFilter.userFilter(student.id)), atomic = false)) { as =>
        as.map(_.asInstanceOf[Authority]).forall { a =>
          !auths.map(_.id).contains(a.id) &&
            a.role == roles.find(_.label == studentRole.label).get.id
        } shouldBe true
      }
    }

    "never delete an authority which is associated with a basic role" in {
      val student = UserDb(UUID.randomUUID.toString, "last", "first", "mail", StudentStatus, None, None)
      val otherUser = UserDb(UUID.randomUUID.toString, "last", "first", "mail", EmployeeStatus, None, None)
      val basicAuth = AuthorityDb(student.id, roles.find(_.label == studentRole.label).get.id)
      val nonBasicAuth = AuthorityDb(student.id, roles.find(_.label == Role.CourseAssistant.label).get.id)

      runAsyncSequence(
        TableQuery[UserTable].forceInsertAll(List(otherUser, student)),
        dao.tableQuery.forceInsertAll(List(basicAuth, nonBasicAuth))
      )

      async(dao.deleteAuthorityIfNotBasic(nonBasicAuth.id))(_.role shouldBe nonBasicAuth.role)
      async(dao.deleteAuthorityIfNotBasic(basicAuth.id).failed)(_.getMessage.containsSlice(studentRole.label) shouldBe true)
      async(dao.get(List(TableFilter.userFilter(student.id)), atomic = false))(_.size == 1)
    }

    "create course manager authority for a given course" in {
      val lecturer = UserDb(UUID.randomUUID.toString, "last", "first", "mail", LecturerStatus, None, None)
      val course1 = CourseDb("course1", "course1", "course1", lecturer.id, 1)
      val course2 = CourseDb("course2", "course2", "course2", lecturer.id, 3)

      runAsyncSequence(
        TableQuery[UserTable].forceInsert(lecturer),
        TableQuery[CourseTable].forceInsertAll(List(course1, course2)),
        dao.createAssociatedAuthorities(course1) map { auth =>
          auth.role shouldBe courseManager.id
          auth.course shouldBe Some(course1.id)
        },
        dao.createAssociatedAuthorities(course2) map { auth =>
          auth.role shouldBe courseManager.id
          auth.course shouldBe Some(course2.id)
        }
      )
    }

    "successfully return if a given user is a course manager" in {
      val lecturer = UserDb(UUID.randomUUID.toString, "last", "first", "mail", LecturerStatus, None, None)
      val course1 = CourseDb("course1", "course1", "course1", lecturer.id, 1)
      val course2 = CourseDb("course2", "course2", "course2", lecturer.id, 3)
      val auths = List(
        AuthorityDb(lecturer.id, employeeRole.id),
        AuthorityDb(lecturer.id, courseManager.id, Some(course1.id)),
        AuthorityDb(lecturer.id, courseManager.id, Some(course2.id)),
        AuthorityDb(lecturer.id, adminRole.id)
      )

      runAsyncSequence(
        TableQuery[UserTable].forceInsert(lecturer),
        TableQuery[CourseTable].forceInsertAll(List(course1, course2)),
        dao.createManyQuery(auths),
        dao.isCourseManager(lecturer.id) map (_ shouldBe true),
        dao.invalidateSingle(auths(1).id),
        dao.isCourseManager(lecturer.id) map (_ shouldBe true),
        dao.invalidateSingle(auths(2).id),
        dao.isCourseManager(lecturer.id) map (_ shouldBe false)
      )
    }

    "successfully return if a given user is not a course manager at all" in {
      val lecturer = UserDb(UUID.randomUUID.toString, "last", "first", "mail", LecturerStatus, None, None)
      val auths = List(
        AuthorityDb(lecturer.id, employeeRole.id),
        AuthorityDb(lecturer.id, adminRole.id)
      )

      runAsyncSequence(
        TableQuery[UserTable].forceInsert(lecturer),
        dao.createManyQuery(auths),
        dao.isCourseManager(lecturer.id) map (_ shouldBe false)
      )
    }

    "delete course manager authority for a given course" in {
      val lecturer = UserDb(UUID.randomUUID.toString, "last", "first", "mail", LecturerStatus, None, None)
      val course1 = CourseDb("course1", "course1", "course1", lecturer.id, 1)
      val course2 = CourseDb("course2", "course2", "course2", lecturer.id, 3)
      val auths = List(
        AuthorityDb(lecturer.id, employeeRole.id),
        AuthorityDb(lecturer.id, courseManager.id, Some(course1.id)),
        AuthorityDb(lecturer.id, courseManager.id, Some(course2.id))
      )

      runAsyncSequence(
        TableQuery[UserTable].forceInsert(lecturer),
        TableQuery[CourseTable].forceInsertAll(List(course1, course2)),
        dao.createManyQuery(auths),
        dao.deleteCourseManagerQuery(course1) map (_ shouldBe 1),
        dao.deleteCourseManagerQuery(course2) map (_ shouldBe 1),
        dao.deleteCourseManagerQuery(course1) map (_ shouldBe 0)
      )
    }

    "delete associated authorities for a given course" in {
      val lecturer = UserDb(UUID.randomUUID.toString, "last", "first", "mail", LecturerStatus, None, None)
      val course1 = CourseDb("course1", "course1", "course1", lecturer.id, 1)
      val course2 = CourseDb("course2", "course2", "course2", lecturer.id, 3)
      val auths = List(
        AuthorityDb(lecturer.id, employeeRole.id),
        AuthorityDb(lecturer.id, courseManager.id, Some(course1.id)),
        AuthorityDb(lecturer.id, courseManager.id, Some(course2.id)),
      )

      runAsyncSequence(
        TableQuery[UserTable].forceInsert(lecturer),
        TableQuery[CourseTable].forceInsertAll(List(course1, course2)),
        dao.createManyQuery(auths),
        dao.deleteAssociatedAuthorities(course1) map (_ shouldBe 1),
        dao.deleteAssociatedAuthorities(course2) map (_ shouldBe 1),
        dao.filterBy(List(TableFilter.userFilter(lecturer.id))).result map (_ shouldBe auths.take(1))
      )
    }

    "update course manager authority for a given course" in {
      val oldLecturer = UserDb(UUID.randomUUID.toString, "last", "first", "mail", LecturerStatus, None, None)
      val newLecturer = UserDb(UUID.randomUUID.toString, "last", "first", "mail", LecturerStatus, None, None)
      val oldCourse1 = CourseDb("course1", "course1", "course1", oldLecturer.id, 1)
      val oldCourse2 = CourseDb("course2", "course2", "course2", oldLecturer.id, 3)
      val newCourse = oldCourse1.copy(lecturer = newLecturer.id, label = "new course")
      val oldLecturerAuths = List(
        AuthorityDb(oldLecturer.id, employeeRole.id),
        AuthorityDb(oldLecturer.id, courseManager.id, Some(oldCourse1.id)),
        AuthorityDb(oldLecturer.id, courseManager.id, Some(oldCourse2.id)),
      )
      val newLecturerAuths = List(
        AuthorityDb(newLecturer.id, employeeRole.id)
      )

      runAsyncSequence(
        TableQuery[UserTable].forceInsertAll(List(oldLecturer, newLecturer)),
        TableQuery[CourseTable].forceInsertAll(List(oldCourse1, oldCourse2)),
        dao.createManyQuery(oldLecturerAuths ++ newLecturerAuths),
        dao.updateAssociatedAuthorities(oldCourse1, newCourse) map {
          case (deletedRows, auth) =>
            deletedRows shouldBe 1

            auth.role shouldBe courseManager.id
            auth.course shouldBe Some(newCourse.id)
        }
      )
    }

    "successfully return all authorities for a user's system id" in {
      val lecturer1 = UserDb(UUID.randomUUID.toString, "last", "first", "mail", LecturerStatus, None, None)
      val lecturer2 = UserDb(UUID.randomUUID.toString, "last", "first", "mail", LecturerStatus, None, None)
      val lecturer3 = UserDb(UUID.randomUUID.toString, "last", "first", "mail", LecturerStatus, None, None)
      val auths = List(
        AuthorityDb(lecturer1.id, employeeRole.id),
        AuthorityDb(lecturer1.id, adminRole.id),
        AuthorityDb(lecturer2.id, employeeRole.id),
        AuthorityDb(lecturer3.id, employeeRole.id),
      )

      runAsyncSequence(
        TableQuery[UserTable].forceInsertAll(List(lecturer1, lecturer2, lecturer3)),
        dao.createManyQuery(auths)
      )

      async(dao.authoritiesFor(lecturer1.systemId))(_ shouldBe auths.filter(_.user == lecturer1.id).map(_.toUniqueEntity))
      async(dao.authoritiesFor(lecturer2.systemId))(_ shouldBe auths.filter(_.user == lecturer2.id).map(_.toUniqueEntity))
      async(dao.authoritiesFor(lecturer3.systemId))(_ shouldBe auths.filter(_.user == lecturer3.id).map(_.toUniqueEntity))
    }
  }


  override protected val dbEntity: AuthorityDb = authorities.head

  override protected val invalidDuplicateOfDbEntity: AuthorityDb = dbEntity.copy(id = UUID.randomUUID)

  override protected val invalidUpdateOfDbEntity: AuthorityDb = dbEntity.copy(role = UUID.randomUUID)

  override protected val validUpdateOnDbEntity: AuthorityDb = dbEntity.copy(lastModified = DateTime.now.plusDays(1).timestamp)

  override protected val dbEntities: List[AuthorityDb] = authorities.tail

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[UserTable].forceInsertAll(employees),
    TableQuery[RoleTable].forceInsertAll(roles),
    TableQuery[CourseTable].forceInsertAll(courses)
  )

  override protected val lwmAtom: AuthorityAtom = {
    val course = for {
      courseId <- dbEntity.course
      courseDb <- courses.find(_.id == courseId)
      employee <- employees.find(_.id == courseDb.lecturer)
    } yield CourseAtom(courseDb.label, courseDb.description, courseDb.abbreviation, employee.toUniqueEntity, courseDb.semesterIndex, courseDb.id)

    AuthorityAtom(
      employees.find(_.id == dbEntity.user).get.toUniqueEntity,
      roles.find(_.id == dbEntity.role).get.toUniqueEntity,
      course,
      dbEntity.id
    )
  }

  override protected def name: String = "authority"

  override protected val dao: AuthorityDao = app.injector.instanceOf(classOf[AuthorityDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
