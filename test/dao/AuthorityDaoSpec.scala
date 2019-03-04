package dao

import database._
import database.helper.{EmployeeStatus, LecturerStatus, StudentStatus}
import models._
import org.joda.time.DateTime
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write

class AuthorityDaoSpec extends AbstractDaoSpec[AuthorityTable, AuthorityDb, AuthorityLike] {

  import scala.concurrent.ExecutionContext.Implicits.global

  import AbstractDaoSpec._
  import slick.jdbc.PostgresProfile.api._
  import utils.LwmDateTime.DateTimeConverter

  "A AuthorityDaoSpec" should {

    "create a associated basic authority for a given user" in {
      val employee = UserDb("id", "last", "first", "mail", EmployeeStatus, None, None)
      val lecturer = UserDb("id", "last", "first", "mail", LecturerStatus, None, None)
      val student = UserDb("id", "last", "first", "mail", StudentStatus, None, None)

      runAsyncSequence(
        TableQuery[UserTable].forceInsertAll(List(employee, lecturer, student)),
        dao.createBasicAuthorityFor(employee) map { auth =>
          auth.user shouldBe employee.id
          auth.role shouldBe roles.find(_.label == Role.EmployeeRole.label).get.id
          auth.course shouldBe empty
        },
        dao.createBasicAuthorityFor(lecturer) map { auth =>
          auth.user shouldBe lecturer.id
          auth.role shouldBe roles.find(_.label == Role.EmployeeRole.label).get.id
          auth.course shouldBe empty
        },
        dao.createBasicAuthorityFor(student) map { auth =>
          auth.user shouldBe student.id
          auth.role shouldBe roles.find(_.label == Role.StudentRole.label).get.id
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

/*    "create Authorities by Course" in {
      val course = privateCourses.head

      await(db.run{
        for{
          _ <- createByCourseQuery(course)
          rm <- roleService.byRoleLabelQuery(Role.RightsManagerLabel) if rm.isDefined
          cm <- roleService.byRoleLabelQuery(Role.CourseManagerLabel) if cm.isDefined
          authoritiesOfLecturer <- tableQuery.filter(_.user === course.lecturer).result

        } yield {
          authoritiesOfLecturer.exists(a => a.role == rm.get.id && a.user == course.lecturer) shouldBe true
          authoritiesOfLecturer.exists(a => a.role == cm.get.id && a.course.get == course.id && a.user == course.lecturer) shouldBe true
        }
      })

    }

    "delete Authorities by Course" in {
      val course = privateCourses(1)

      await(db.run{
        for{
          _ <- createByCourseQuery(course)
          rm <- roleService.byRoleLabelQuery(Role.RightsManagerLabel) if rm.isDefined
          cm <- roleService.byRoleLabelQuery(Role.CourseManagerLabel) if cm.isDefined
          _ <- deleteByCourseQuery(course)
          authoritiesOfLecturer <- tableQuery.filter(_.user === course.lecturer).result

        } yield {
          if(authoritiesOfLecturer.exists(a => a.role == cm.get.id)){
            authoritiesOfLecturer.exists(a => a.role == rm.get.id && a.user == course.lecturer) shouldBe true
          }else{
            authoritiesOfLecturer.exists(a => a.role == rm.get.id && a.user == course.lecturer) shouldBe false
          }
          authoritiesOfLecturer.exists(a => a.role == cm.get.id && a.course.get == course.id && a.user == course.lecturer) shouldBe false
        }
      })
    }

    "update Authorities by Course" in {
      val course = privateCourses(2)
      val newLecturer = privateLecturers(6)
      val sameCourseWithNewLecturer = course.copy(lecturer = newLecturer.id)

      await(db.run{
        for{
          _ <- createByCourseQuery(course)
          authoritiesOfOldLecturerBeforeUpdate <- tableQuery.filter(_.user === course.lecturer).result
          authoritiesOfNewLecturerBeforeUpdate <- tableQuery.filter(_.user === sameCourseWithNewLecturer.lecturer).result
          _ <- updateByCourseQuery(course, sameCourseWithNewLecturer)
          authoritiesOfOldLecturerAfterUpdate <- tableQuery.filter(_.user === course.lecturer).result
          authoritiesOfNewLecturerAfterUpdate <- tableQuery.filter(_.user === sameCourseWithNewLecturer.lecturer).result

          rm <- roleService.byRoleLabelQuery(Role.RightsManagerLabel) if rm.isDefined
          cm <- roleService.byRoleLabelQuery(Role.CourseManagerLabel) if cm.isDefined
        } yield{
          authoritiesOfOldLecturerBeforeUpdate.count(_.role === rm.get.id) shouldBe 1
          authoritiesOfOldLecturerBeforeUpdate.count(_.role === cm.get.id) shouldBe 1
          authoritiesOfNewLecturerBeforeUpdate.count(_.role === rm.get.id) shouldBe 0
          authoritiesOfNewLecturerBeforeUpdate.count(_.role === cm.get.id) shouldBe 0

          authoritiesOfOldLecturerAfterUpdate.count(_.role === rm.get.id) shouldBe 0
          authoritiesOfOldLecturerAfterUpdate.count(_.role === cm.get.id) shouldBe 0
          authoritiesOfNewLecturerAfterUpdate.count(_.role === rm.get.id) shouldBe 1
          authoritiesOfNewLecturerAfterUpdate.count(_.role === cm.get.id) shouldBe 1
        }
      })

    }

    "not duplicate RightsManager for one User" in {
      val course1 = privateCoursesWithSameLecturer.head
      val course2 = privateCoursesWithSameLecturer(1)

      await(db.run{
        for{
          _ <- createByCourseQuery(course1)
          _ <- createByCourseQuery(course2)
          rm <- roleService.byRoleLabelQuery(Role.RightsManagerLabel) if rm.isDefined
          authoritiesOfLecturer <- tableQuery.filter(_.user === course1.lecturer).result
        } yield{
          authoritiesOfLecturer.count(_.role === rm.get.id) shouldBe 1
        }
      })
    }

    "delete RightsManager if the User doesn't have other CourseManagers" in {
      val course1 = privateCoursesWithSameLecturer2.head
      val course2 = privateCoursesWithSameLecturer2(1)

      await(db.run{
        for{
          _ <- createByCourseQuery(course1)
          _ <- createByCourseQuery(course2)
          _ <- deleteByCourseQuery(course1)
          authoritiesOfLecturerAfterDelete1 <- tableQuery.filter(_.user === course1.lecturer).result
          _ <- deleteByCourseQuery(course2)
          authoritiesOfLecturerAfterDelete2 <- tableQuery.filter(_.user === course1.lecturer).result
          rm <- roleService.byRoleLabelQuery(Role.RightsManagerLabel) if rm.isDefined
        } yield{
          authoritiesOfLecturerAfterDelete1.count(_.role === rm.get.id) shouldBe 1
          authoritiesOfLecturerAfterDelete2.count(_.role === rm.get.id) shouldBe 0
        }
      })
    }

    "not delete RightsManager if the User has other CourseManagers" in {
      val course1 = privateCoursesWithSameLecturer2.head
      val course2 = privateCoursesWithSameLecturer2(1)

      await(db.run{
        for{
          _ <- createByCourseQuery(course1)
          _ <- createByCourseQuery(course2)
          _ <- deleteByCourseQuery(course1)
          authoritiesOfLecturerAfterDelete1 <- tableQuery.filter(_.user === course1.lecturer).result
          _ <- deleteByCourseQuery(course2)
          authoritiesOfLecturerAfterDelete2 <- tableQuery.filter(_.user === course1.lecturer).result
          rm <- roleService.byRoleLabelQuery(Role.RightsManagerLabel) if rm.isDefined
        } yield{
          authoritiesOfLecturerAfterDelete1.count(_.role === rm.get.id) shouldBe 1
          authoritiesOfLecturerAfterDelete2.count(_.role === rm.get.id) shouldBe 0
        }
      })
    }

    "check authorities properly" in {
      val modules = populateCourses(2)(i => i)
      val module1 = modules.head.id
      val module2 = modules.last.id

      val role1 = RoleDb(Role.CourseEmployee.label) // suff
      val role2 = RoleDb(Role.CourseAssistant.label) // insuff
      val role3 = RoleDb(Role.Admin.label) // admin

      val roles = List(role1, role2, role3)

      val students = populateStudents(6)

      val noneModule1Role1 = AuthorityDb(students(0).id, role1.id)

      val module1UserRole1 = AuthorityDb(students(2).id, role1.id, Some(module1))
      val module1UserRole2 = AuthorityDb(students(3).id, role2.id, Some(module1))
      val module2UserRole2 = AuthorityDb(students(4).id, role2.id, Some(module2))
      val adminRefRole = AuthorityDb(students(5).id, role3.id)

      run(DBIO.seq(
        TableQuery[AuthorityTable].delete,
        TableQuery[RoleTable].delete,
        TableQuery[DegreeTable].delete,
        TableQuery[CourseTable].delete,
        TableQuery[DegreeTable].forceInsertAll(degrees),
        TableQuery[CourseTable].forceInsertAll(modules),
        TableQuery[UserTable].forceInsertAll(students)
      ))

      await(roleService.createMany(roles))

      val result1 = checkAuthority((Some(module1), List(Role.CourseEmployee)))(Seq(module1UserRole2.toUniqueEntity))
      val result2 = checkAuthority((Some(module1), List(Role.CourseEmployee)))(Seq(module1UserRole1, module2UserRole2).map(_.toUniqueEntity))
      val result3 = checkAuthority((None, List(Role.CourseEmployee)))(Seq(module1UserRole1, noneModule1Role1, module2UserRole2).map(_.toUniqueEntity))
      val result4 = checkAuthority((Some(module1), List(Role.CourseEmployee)))(Seq(adminRefRole.toUniqueEntity))
      val result5 = checkAuthority((Some(module2), List(Role.CourseAssistant)))(Seq(module1UserRole1.toUniqueEntity))
      val result6 = checkAuthority((Some(UUID.randomUUID()), List(Role.CourseEmployee)))(Seq(adminRefRole.toUniqueEntity))
      val result7 = checkAuthority((Some(module1), List(Role.CourseAssistant, Role.CourseManager)))(Seq(module1UserRole2.toUniqueEntity))
      val result8 = checkAuthority((Some(module1), List(Role.EmployeeRole, Role.CourseManager)))(Seq(module1UserRole2.toUniqueEntity))
      val result9 = checkAuthority((Some(module1), List(Role.God)))(Seq(module1UserRole1, noneModule1Role1, module2UserRole2).map(_.toUniqueEntity))

      await(result1) shouldBe false
      await(result2) shouldBe true
      await(result3) shouldBe true
      await(result4) shouldBe true
      await(result5) shouldBe false
      await(result7) shouldBe true
      await(result8) shouldBe false
      await(result9) shouldBe false
    }*/
  }


  override protected val dbEntity: AuthorityDb = AuthorityDb(randomEmployee.id, roles.find(_.label == Role.EmployeeRole.label).get.id)

  override protected val invalidDuplicateOfDbEntity: AuthorityDb = AuthorityDb(dbEntity.user, dbEntity.role, dbEntity.course)

  override protected val invalidUpdateOfDbEntity: AuthorityDb = dbEntity

  override protected val validUpdateOnDbEntity: AuthorityDb = dbEntity.copy(lastModified = DateTime.now.plusDays(1).timestamp)

  override protected val dbEntities: List[AuthorityDb] = authorities

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[UserTable].forceInsertAll(employees),
    TableQuery[RoleTable].forceInsertAll(roles),
    TableQuery[CourseTable].forceInsertAll(courses)
  )

  override protected val lwmAtom: AuthorityAtom = {
    val course = for {
      courseId <- dbEntity.course
      courseDb <- courses.find(_.id == courseId)
      employee <- employees.find( _.id == courseDb.lecturer)
    } yield CourseAtom(courseDb.label, courseDb.description, courseDb.abbreviation, employee.toUniqueEntity, courseDb.semesterIndex, courseDb.id)

    AuthorityAtom(
      employees.find(_.id == dbEntity.user).get.toUniqueEntity,
      roles.find(_.id == dbEntity.role).get.toUniqueEntity,
      course,
      dbEntity.id
    )
  }

  override protected def name: String = "authority"

  override protected def dao: AuthorityDao = app.injector.instanceOf(classOf[AuthorityDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
