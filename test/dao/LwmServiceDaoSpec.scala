package dao

import java.util.UUID

import base.PostgresDbSpec
import models._
import slick.driver.PostgresDriver.api._
import store._

final class LwmServiceDaoSpec extends PostgresDbSpec with LwmServiceDao {

  import dao.AbstractDaoSpec._
  import models.LwmDateTime._

  val (privateCourses, privateDegrees, privateStudents, privateLabs, privateGroups) = groupStack(100, 10)

  val lapps = privateGroups.flatMap(g => g.members.map(s => LabworkApplicationDb(g.labwork, s, Set.empty)))
  val cards = lapps.flatMap(l => populateReportCardEntries(1, 2, withRescheduledAndRetry = false)
  (privateLabs.filter(_.id == l.labwork), privateStudents.filter(_.id == l.applicant)))

  private def sorted(seq: Seq[PostgresReportCardEntry]) = seq.sortBy(c => c.date.toLocalDateTime(c.start))

  private def reportCardEntriesFromDb(student: UUID, labwork: UUID) = await(
    reportCardEntryDao.get(List(ReportCardEntryStudentFilter(student.toString), ReportCardEntryLabworkFilter(labwork.toString)), atomic = false)
  ).map(_.asInstanceOf[PostgresReportCardEntry])

  private def groupFromDb(student: UUID, labwork: UUID) = await(
    groupDao.get(List(GroupLabworkTableFilter(labwork.toString), GroupStudentTableFilter(student.toString)), atomic = false)
  ).map(_.asInstanceOf[PostgresGroup])

  "A LwmServiceDaoSpec" should {
    "add students to existing/running labwork" in {
      val student = DbUser("toAdd", "toAdd", "toAdd", "toAdd", User.StudentType, Some("toAdd"), Some(takeOneOf(privateDegrees).id))
      val labwork = privateLabs.find(_.degree == student.enrollment.get).get
      val group = privateGroups.find(_.labwork == labwork.id).get
      run(TableQuery[UserTable].forceInsert(student)) shouldBe 1

      val (lapp, membership, reportCards) = await(addStudentToLabwork(student.id, labwork.id, group.id))

      lapp.applicant shouldBe student.id
      lapp.labwork shouldBe labwork.id
      await(labworkApplicationDao.getById(lapp.id.toString, atomic = false)) shouldBe Some(lapp.toLwmModel)

      membership.student shouldBe student.id
      membership.group shouldBe group.id
      await(groupDao.getById(group.id.toString, atomic = false)) shouldBe Some(group.toLwmModel.copy(members = group.members + student.id))

      val cardsFromDestStudent = reportCardEntriesFromDb(group.members.head, group.labwork)

      cardsFromDestStudent.size shouldBe reportCards.size
      sorted(reportCards.map(_.toLwmModel)).zip(sorted(cardsFromDestStudent)).foreach {
        case (c, d) =>
          c.student shouldBe student.id
          c.labwork shouldBe labwork.id
          (c.rescheduled.isEmpty && c.retry.isEmpty) shouldBe true
          c.entryTypes.forall(t =>
            t.int == 0 && t.bool.isEmpty &&
              !d.entryTypes.exists(_.id == t.id)
          ) shouldBe true

          (c.date.isEqual(d.date) &&
            c.start.withMillisOfSecond(0).isEqual(d.start.withMillisOfSecond(0)) &&
            c.end.withMillisOfSecond(0).isEqual(d.end.withMillisOfSecond(0))) shouldBe true

          c.id should not be d.id
      }
    }

    "swap student in group" in {
      val student = takeOneOf(privateStudents)
      val labwork = privateLabs.find(_.degree == student.enrollment.get).get
      val group = privateGroups.filter(g => g.labwork == labwork.id && !g.members.contains(student.id)).last
      val oldCardsDb = reportCardEntriesFromDb(student.id, labwork.id)
      val oldGroupDb = groupFromDb(student.id, labwork.id)

      val (membership, oldCards, newCards) = await(swapStudentsInGroup(student.id, labwork.id, group.id))

      membership.student shouldBe student.id
      membership.group shouldBe group.id

      newCards.map(_.isDefined).size shouldBe oldCards.size

      val newCardsDb = reportCardEntriesFromDb(student.id, labwork.id)
      val newGroupDb = groupFromDb(student.id, labwork.id)
      val newCardsUnwrapped = newCards.map(_.get)
      oldCards.map(_.toLwmModel) shouldBe oldCardsDb
      sorted(newCardsUnwrapped.map(_.toLwmModel)) shouldBe sorted(newCardsDb)
      oldGroupDb.size shouldBe 1
      newGroupDb.size shouldBe 1
      oldGroupDb should not be newGroupDb

      sorted(newCardsUnwrapped).zip(sorted(oldCards)).foreach {
        case (n, o) =>
          n.student shouldBe o.student
          n.labwork shouldBe o.labwork
          n.rescheduled shouldBe o.rescheduled
          n.retry shouldBe o.retry
          n.entryTypes shouldBe o.entryTypes
          n.id shouldBe o.id

          (n.room == o.room &&
            n.date.localDate.isEqual(o.date.localDate) &&
            n.start.localTime.withMillisOfSecond(0).isEqual(o.start.localTime.withMillisOfSecond(0)) &&
            n.end.localTime.withMillisOfSecond(0).isEqual(o.end.localTime.withMillisOfSecond(0))) shouldBe false
      }
    }
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    await(labworkApplicationDao.createMany(lapps))
    await(groupDao.createMany(privateGroups))
    await(reportCardEntryDao.createMany(cards))
  }

  override protected val dependencies = DBIO.seq(
    TableQuery[UserTable].forceInsertAll(employees),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[RoomTable].forceInsertAll(rooms),
    TableQuery[DegreeTable].forceInsertAll(privateDegrees),
    TableQuery[UserTable].forceInsertAll(privateStudents),
    TableQuery[CourseTable].forceInsertAll(privateCourses),
    TableQuery[LabworkTable].forceInsertAll(privateLabs)
  )

  override protected val labworkApplicationDao: LabworkApplicationDao = {
    val sharedDb = db

    new LabworkApplicationDao {
      override protected val db = sharedDb
    }
  }

  override protected val groupDao: GroupDao = {
    val sharedDb = db

    new GroupDao {
      override protected val db = sharedDb
    }
  }

  override protected val reportCardEntryDao: ReportCardEntryDao = {
    val sharedDb = db

    new ReportCardEntryDao {
      override protected val db = sharedDb
    }
  }
}
