package dao

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import base.{PostgresDbSpec, TestBaseDefinition}
import database._
import database.helper.LdapUserStatus
import models.{LabworkAtom, ScheduleEntryLike}
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}

final class ScheduleEntryDaoSpec extends PostgresDbSpec with TestBaseDefinition {

  import profile.api._

  implicit val ctx: ExecutionContext = app.injector.instanceOf(classOf[ExecutionContext])

  val dao = app.injector.instanceOf(classOf[ScheduleEntryDao])
  val labworkDao = app.injector.instanceOf(classOf[LabworkDao])

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    clearTables()
  }

  "A ScheduleEntryDaoSpec" should {

    "return empty competitive entries if given labwork is the only one" in {
      val labwork = populateLabwork
      populateScheduleEntries(1, labwork.id)

      val result = competitive(labwork.id, considerSemesterIndex = true)

      async(result)(_ shouldBe empty)
    }

    "return empty competitive entries if given labwork is the only valid one" in {
      val labwork1 = populateLabwork
      val labwork2 = populateLabwork
      populateScheduleEntries(1, labwork1.id)
      populateScheduleEntries(1, labwork2.id)

      val result = for {
        _ <- labworkDao.invalidate(labwork2)
        comps <- competitive(labwork1.id, considerSemesterIndex = true)
      } yield comps

      async(result)(_ shouldBe empty)
    }

    "return empty competitive entries if those are invalid ones" in {
      val labwork1 = populateLabwork
      val labwork2 = populateLabwork
      populateScheduleEntries(1, labwork1.id)
      val scheduleEntries = populateScheduleEntries(1, labwork2.id)

      val result = for {
        _ <- dao.invalidateMany(scheduleEntries.map(_.id).toList)
        comps <- competitive(labwork1.id, considerSemesterIndex = true)
      } yield comps

      async(result)(_ shouldBe empty)
    }

    "return competitive entries of all labworks within a given course with the same semester index" in {
      val labwork1 = populateLabwork
      val labwork2 = createCopy(labwork1)
      val labwork3 = createCopy(labwork1)
      populateScheduleEntries(1, labwork1.id)
      val comp1 = populateScheduleEntries(1, labwork2.id).toList
      val comp2 = populateScheduleEntries(1, labwork3.id).toList

      val result = competitive(labwork1.id, considerSemesterIndex = true)

      async(result)(_ should contain theSameElementsAs (comp1 ::: comp2).map(_.toUniqueEntity))
    }

    "return competitive entries of all labworks within a given course with the same semester index as long as they are valid" in {
      val labwork1 = populateLabwork
      val labwork2 = createCopy(labwork1)
      val labwork3 = createCopy(labwork1)
      populateScheduleEntries(1, labwork1.id)
      val comp1 = populateScheduleEntries(1, labwork2.id).toList
      val comp2 = populateScheduleEntries(1, labwork3.id).toList

      val result = for {
        _ <- labworkDao.invalidate(labwork2)
        comps <- competitive(labwork1.id, considerSemesterIndex = true)
      } yield comps

      async(result)(_ should contain theSameElementsAs comp2.map(_.toUniqueEntity))
    }

    "return competitive entries of all courses with the same semester index" in {
      val labwork1 = populateLabwork

      val user1 = UserDb("", "", "", "", LdapUserStatus.EmployeeStatus, None, None)
      val course1 = CourseDb("", "", "", user1.id, 2)
      val user2 = UserDb("", "", "", "", LdapUserStatus.EmployeeStatus, None, None)
      val course2 = CourseDb("", "", "", user2.id, 1)
      val user3 = UserDb("", "", "", "", LdapUserStatus.EmployeeStatus, None, None)
      val course3 = CourseDb("", "", "", user3.id, 1)

      val labwork2 = labwork1.copy(id = UUID.randomUUID, course = course1.id)
      val labwork3 = labwork1.copy(id = UUID.randomUUID, course = course2.id)
      val labwork4 = labwork1.copy(id = UUID.randomUUID, course = course3.id, invalidated = Some(new Timestamp(0)))

      runAsyncSequence(
        TableQuery[UserTable].forceInsertAll(List(user1, user2, user3)),
        TableQuery[CourseTable].forceInsertAll(List(course1, course2, course3)),
        TableQuery[LabworkTable].forceInsertAll(List(labwork2, labwork3, labwork4)),
      )

      populateScheduleEntries(1, labwork1.id)
      populateScheduleEntries(1, labwork2.id)
      val comp = populateScheduleEntries(1, labwork3.id).toList
      populateScheduleEntries(1, labwork4.id)

      val result = competitive(labwork1.id, considerSemesterIndex = true)

      async(result)(_ should contain theSameElementsAs comp.map(_.toUniqueEntity))
    }

    "return competitive entries of all courses regardless of semester index" in {
      val labwork1 = populateLabwork

      val user1 = UserDb("", "", "", "", LdapUserStatus.EmployeeStatus, None, None)
      val course1 = CourseDb("", "", "", user1.id, 2)
      val user2 = UserDb("", "", "", "", LdapUserStatus.EmployeeStatus, None, None)
      val course2 = CourseDb("", "", "", user2.id, 5)
      val user3 = UserDb("", "", "", "", LdapUserStatus.EmployeeStatus, None, None)
      val course3 = CourseDb("", "", "", user3.id, 8)
      val user4 = UserDb("", "", "", "", LdapUserStatus.EmployeeStatus, None, None)
      val course4 = CourseDb("", "", "", user4.id, 8)

      val labwork2 = labwork1.copy(id = UUID.randomUUID, course = course1.id)
      val labwork3 = labwork1.copy(id = UUID.randomUUID, course = course2.id)
      val labwork4 = labwork1.copy(id = UUID.randomUUID, course = course3.id, invalidated = Some(new Timestamp(0)))
      val labwork5 = labwork1.copy(id = UUID.randomUUID, course = course4.id)

      runAsyncSequence(
        TableQuery[UserTable].forceInsertAll(List(user1, user2, user3, user4)),
        TableQuery[CourseTable].forceInsertAll(List(course1, course2, course3, course4)),
        TableQuery[LabworkTable].forceInsertAll(List(labwork2, labwork3, labwork4, labwork5)),
      )

      populateScheduleEntries(1, labwork1.id)
      val comp1 = populateScheduleEntries(1, labwork2.id).toList
      val comp2 = populateScheduleEntries(1, labwork3.id).toList
      populateScheduleEntries(1, labwork4.id)
      val comp3 = populateScheduleEntries(1, labwork5.id).toList

      val result = competitive(labwork1.id, considerSemesterIndex = false)

      async(result)(_ should contain theSameElementsAs (comp1 ::: comp2 ::: comp3).map(_.toUniqueEntity))
    }
  }

  private def createCopy(labwork: LabworkDb): LabworkDb = {
    val labwork2 = labwork.copy(id = UUID.randomUUID)
    runAsync(TableQuery[LabworkTable].forceInsert(labwork2))(_ => Unit)
    labwork2
  }

  private def competitive(labwork: UUID, considerSemesterIndex: Boolean): Future[Seq[ScheduleEntryLike]] = {
    for {
      labworkAtom <- labworkDao.getSingle(labwork)
      comps <- dao.competitive(
        labworkAtom.get.asInstanceOf[LabworkAtom],
        atomic = false,
        considerSemesterIndex = considerSemesterIndex
      )
    } yield comps
  }

  private def populateLabwork: LabworkDb = {
    val semester = SemesterDb("", "", fakeDate, fakeDate, fakeDate)
    val user = UserDb("", "", "", "", LdapUserStatus.EmployeeStatus, None, None)
    val course = CourseDb("", "", "", user.id, 1)
    val degree = DegreeDb("", "")
    val labwork = LabworkDb("", "", semester.id, course.id, degree.id)

    runAsyncSequence(
      TableQuery[SemesterTable].forceInsert(semester),
      TableQuery[UserTable].forceInsert(user),
      TableQuery[CourseTable].forceInsert(course),
      TableQuery[DegreeTable].forceInsert(degree),
      TableQuery[LabworkTable].forceInsert(labwork)
    )

    labwork
  }

  private def populateScheduleEntries(n: Int, labwork: UUID): immutable.Seq[ScheduleEntryDb] = {
    val room = RoomDb("", "", 1)
    val grp = GroupDb("", labwork, Set.empty)
    val scheduleEntries = (0 until n).map { _ =>
      ScheduleEntryDb(labwork, fakeTime, fakeTime, fakeDate, room.id, Set.empty, grp.id)
    }

    runAsyncSequence(
      TableQuery[RoomTable].forceInsert(room),
      TableQuery[GroupTable].forceInsert(grp),
      TableQuery[ScheduleEntryTable].forceInsertAll(scheduleEntries)
    )

    scheduleEntries
  }

  private def fakeTime: Time = new Time(0)

  private def fakeDate: Date = new Date(0)

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
