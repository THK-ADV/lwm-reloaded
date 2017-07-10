package services

import models._
import org.joda.time.DateTime
import slick.dbio.Effect.Write
import slick.driver
import store.{AuthorityTable, CourseTable, RoleTable, UserTable}

/**
  * Created by florian on 4/10/17.
  */
class AuthorityServiceSpec extends AbstractDaoSpec[AuthorityTable, AuthorityDb, Authority] with AuthorityService {

  import services.AbstractDaoSpec._
  import slick.driver.PostgresDriver.api._
  import models.LwmDateTime.DateTimeConverter

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
          _ <- createByCourse(course)
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
          _ <- createByCourse(course)
          rm <- roleService.byRoleLabelQuery(Roles.RightsManagerLabel) if rm.isDefined
          cm <- roleService.byRoleLabelQuery(Roles.CourseManagerLabel) if cm.isDefined
          _ <- deleteByCourse(course)
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
          _ <- createByCourse(course)
          authoritiesOfOldLecturerBeforeUpdate <- tableQuery.filter(_.user === course.lecturer).result
          authoritiesOfNewLecturerBeforeUpdate <- tableQuery.filter(_.user === sameCourseWithNewLecturer.lecturer).result
          _ <- updateByCourse(course, sameCourseWithNewLecturer)
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
          _ <- createByCourse(course1)
          _ <- createByCourse(course2)
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
          _ <- createByCourse(course1)
          _ <- createByCourse(course2)
          _ <- deleteByCourse(course1)
          authoritiesOfLecturerAfterDelete1 <- tableQuery.filter(_.user === course1.lecturer).result
          _ <- deleteByCourse(course2)
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
          _ <- createByCourse(course1)
          _ <- createByCourse(course2)
          _ <- deleteByCourse(course1)
          authoritiesOfLecturerAfterDelete1 <- tableQuery.filter(_.user === course1.lecturer).result
          _ <- deleteByCourse(course2)
          authoritiesOfLecturerAfterDelete2 <- tableQuery.filter(_.user === course1.lecturer).result
          rm <- roleService.byRoleLabelQuery(Roles.RightsManagerLabel) if rm.isDefined
        } yield{
          authoritiesOfLecturerAfterDelete1.count(_.role === rm.get.id) shouldBe 1
          authoritiesOfLecturerAfterDelete2.count(_.role === rm.get.id) shouldBe 0
        }
      })
    }



    //"create a course with dedicated roles" in {
//
    //  val lecturer = randomEmployee
//
    //  await(db.run(
    //    for {
    //      result <- createManyExpandedQuery(Seq(CourseDb("TestLabel", "TestDescription", "TL", lecturer.id, 3))).map(_.head)
    //      auths <- authorityService.tableQuery.filter(_.user === lecturer.id).result
    //      rightsManager <- getRightsManager
    //      courseManager <- getCourseManager
    //    } yield {
    //      auths.exists(a => a.user == result.lecturer && a.role == rightsManager.get.id && a.course.isEmpty) shouldBe true
    //      auths.exists(a => a.user == result.lecturer && a.role == courseManager.get.id && a.course.contains(result.id)) shouldBe true
    //    }
    //  ))
    //}
//
    //"update a course with dedicated roles" in {
    //  val oldLecturer = employees(0)
    //  val newLecturer = employees(1)
//
    //  val course = CourseDb("TestLabel2", "TestDescription2", "TL2", oldLecturer.id, 3)
//
    //  await(db.run(
    //    for {
    //      oldCourse <- createManyExpandedQuery(Seq(course)).map(_.head)
    //      _ <- updateExpandedQuery(oldCourse.copy(oldCourse.label, oldCourse.description, oldCourse.abbreviation, newLecturer.id, oldCourse.semesterIndex))
//
    //      authOldLecturer <- authorityService.tableQuery.filter(_.user === oldLecturer.id).result
    //      authNewLecturer <- authorityService.tableQuery.filter(_.user === newLecturer.id).result
//
    //      courseManager <- getCourseManager
    //    } yield {
    //      authOldLecturer.exists(a => a.role == courseManager.get.id && a.course.contains(oldCourse.id)) shouldBe false
    //      authNewLecturer.exists(a => a.role == courseManager.get.id && a.course.contains(oldCourse.id)) shouldBe true
    //    }
    //  ))
    //}
//
    //"delete a course with dedicated roles" in {
    //  val course = randomCourse
//
    //  await(db.run(
    //    for {
    //      _ <- deleteExpandedQuery(course)
    //      auths <- authorityService.tableQuery.filter(_.user === course.lecturer).result
    //      rightsManager <- getRightsManager
    //      courseManager <- getCourseManager
    //    } yield {
    //      auths.exists(a => a.user == course.lecturer && a.role == courseManager.get.id && a.course.contains(course.id)) shouldBe false
    //      val hasOtherCourses = auths.exists(a => a.user == course.lecturer && a.role == courseManager.get.id)
    //      auths.exists(a => a.role == rightsManager.get.id) shouldBe hasOtherCourses
    //    }))
//
    //}
//
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

  override protected val lwmEntity: Authority = dbEntity.toAuthority

  override protected val lwmAtom: PostgresAuthorityAtom = {

    val course = for{
      courseId <- dbEntity.course
      courseDb <- courses.find(_.id == courseId)
      employee <- employees.find( _.id == courseDb.lecturer)
    } yield PostgresCourseAtom(courseDb.label, courseDb.description, courseDb.abbreviation, employee.toUser, courseDb.semesterIndex, courseDb.id)

    PostgresAuthorityAtom(
      employees.find(_.id == dbEntity.user).get.toUser,
      roles.find(_.id == dbEntity.role).get.toRole,
      course,
      dbEntity.id
    )
  }

  override protected def name: String = "authority"

  override protected def roleService: RoleService2 = {
    val sharedDb = db
    new RoleService2 {
      override protected def db: driver.PostgresDriver.backend.Database = sharedDb
    }
  }
}
