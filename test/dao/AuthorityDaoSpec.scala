package dao

import models._
import org.joda.time.DateTime
import slick.dbio.Effect.Write
import slick.driver
import store.{AuthorityTable, CourseTable, RoleTable, UserTable}

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

  override protected def roleService: RoleDao = {
    val sharedDb = db
    new RoleDao {
      override protected def db: driver.PostgresDriver.backend.Database = sharedDb
    }
  }
}
