package dao

import java.util.UUID

import models._
import org.joda.time.DateTime
import slick.dbio.Effect.Write
import slick.driver
import store._

import scala.util.Random.nextInt

class AuthorityDaoSpec extends AbstractDaoSpec[AuthorityTable, AuthorityDb, Authority] with AuthorityDao {

  import dao.AbstractDaoSpec._
  import models.LwmDateTime.DateTimeConverter
  import slick.driver.PostgresDriver.api._

  private lazy val privateLecturers = (0 until 7).map { i =>
    DbUser(i.toString, i.toString, i.toString, i.toString, User.EmployeeType, None, None)
  }.toList

  private lazy val privateCourses = (0 until 3).map { i =>
    CourseDb(s"testCourse$i", s"testCourse$i", s"tC$i", privateLecturers(i).id, 1)
  }.toList

  private lazy val privateCoursesWithSameLecturer = (0 until 2).map { i =>
    CourseDb(s"testCourse$i", s"testCourse$i", s"tC$i", privateLecturers(3).id, 1)
  }.toList

  private lazy val privateCoursesWithSameLecturer2 = (0 until 2).map { i =>
    CourseDb(s"testCourse$i", s"testCourse$i", s"tC$i", privateLecturers(4).id, 1)
  }.toList

  private lazy val privateCoursesWithSameLecturer3 = (0 until 2).map { i =>
    CourseDb(s"testCourse$i", s"testCourse$i", s"tC$i", privateLecturers(5).id, 1)
  }.toList

  "A AuthorityServiceSpec " should {

    "create Authorities by Course" in {
      val course = privateCourses.head

      await(db.run{
        for{
          _ <- createByCourseQuery(course)
          rm <- roleService.byRoleLabelQuery(Roles.RightsManagerLabel) if rm.isDefined
          cm <- roleService.byRoleLabelQuery(Roles.CourseManagerLabel) if cm.isDefined
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
          rm <- roleService.byRoleLabelQuery(Roles.RightsManagerLabel) if rm.isDefined
          cm <- roleService.byRoleLabelQuery(Roles.CourseManagerLabel) if cm.isDefined
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

          rm <- roleService.byRoleLabelQuery(Roles.RightsManagerLabel) if rm.isDefined
          cm <- roleService.byRoleLabelQuery(Roles.CourseManagerLabel) if cm.isDefined
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
          rm <- roleService.byRoleLabelQuery(Roles.RightsManagerLabel) if rm.isDefined
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
          rm <- roleService.byRoleLabelQuery(Roles.RightsManagerLabel) if rm.isDefined
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
          rm <- roleService.byRoleLabelQuery(Roles.RightsManagerLabel) if rm.isDefined
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

      val students = populateStudents(6)(degrees)

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

      val result1 = checkAuthority((Some(module1), List(Role.CourseEmployee)))(Seq(module1UserRole2.toLwmModel))
      val result2 = checkAuthority((Some(module1), List(Role.CourseEmployee)))(Seq(module1UserRole1, module2UserRole2).map(_.toLwmModel))
      val result3 = checkAuthority((None, List(Role.CourseEmployee)))(Seq(module1UserRole1, noneModule1Role1, module2UserRole2).map(_.toLwmModel))
      val result4 = checkAuthority((Some(module1), List(Role.CourseEmployee)))(Seq(adminRefRole.toLwmModel))
      val result5 = checkAuthority((Some(module2), List(Role.CourseAssistant)))(Seq(module1UserRole1.toLwmModel))
      val result6 = checkAuthority((Some(UUID.randomUUID()), List(Role.CourseEmployee)))(Seq(adminRefRole.toLwmModel))
      val result7 = checkAuthority((Some(module1), List(Role.CourseAssistant, Role.CourseManager)))(Seq(module1UserRole2.toLwmModel))
      val result8 = checkAuthority((Some(module1), List(Role.Employee, Role.CourseManager)))(Seq(module1UserRole2.toLwmModel))
      val result9 = checkAuthority((Some(module1), List(Role.God)))(Seq(module1UserRole1, noneModule1Role1, module2UserRole2).map(_.toLwmModel))

      await(result1) shouldBe false
      await(result2) shouldBe true
      await(result3) shouldBe true
      await(result4) shouldBe true
      await(result5) shouldBe false
      await(result7) shouldBe true
      await(result8) shouldBe false
      await(result9) shouldBe false
    }
  }


  override protected val dbEntity: AuthorityDb = AuthorityDb(randomEmployee.id, roles.find(_.label == Roles.RightsManagerLabel).get.id, Some(randomCourse.id))
  override protected val invalidDuplicateOfDbEntity: AuthorityDb = AuthorityDb(dbEntity.user, dbEntity.role, dbEntity.course)
  override protected val invalidUpdateOfDbEntity: AuthorityDb = dbEntity
  override protected val validUpdateOnDbEntity: AuthorityDb = dbEntity.copy(lastModified = DateTime.now.plusDays(1).timestamp)
  override protected val dbEntities: List[AuthorityDb] = authorities
  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[UserTable].forceInsertAll(employees ++ privateLecturers),
    TableQuery[RoleTable].forceInsertAll(roles),
    TableQuery[CourseTable].forceInsertAll(courses ++ privateCourses ++ privateCoursesWithSameLecturer ++ privateCoursesWithSameLecturer2 ++ privateCoursesWithSameLecturer3)
  )

  override protected val lwmEntity: Authority = dbEntity.toLwmModel

  override protected val lwmAtom: PostgresAuthorityAtom = {
    val course = for{
      courseId <- dbEntity.course
      courseDb <- courses.find(_.id == courseId)
      employee <- employees.find( _.id == courseDb.lecturer)
    } yield PostgresCourseAtom(courseDb.label, courseDb.description, courseDb.abbreviation, employee.toLwmModel, courseDb.semesterIndex, courseDb.id)

    PostgresAuthorityAtom(
      employees.find(_.id == dbEntity.user).get.toLwmModel,
      roles.find(_.id == dbEntity.role).get.toLwmModel,
      course,
      dbEntity.id
    )
  }

  override protected def name: String = "authority"

  override protected val roleService: RoleDao = {
    val sharedDb = db
    new RoleDao {
      override protected def db: driver.PostgresDriver.backend.Database = sharedDb
    }
  }
}
