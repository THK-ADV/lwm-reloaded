package dao

import base.PostgresDbSpec
import models._
import slick.driver.PostgresDriver.api._
import store._

final class LwmServiceDaoSpec extends PostgresDbSpec with LwmServiceDao {

  import dao.AbstractDaoSpec._
  import models.LwmDateTime._

  private val privateCourses = populateCourses(4)(_ % 6)
  private val privateDegrees = populateDegrees(4)
  private val privateStudents = populateStudents(500)(privateDegrees)
  private val privateLabs = populateLabworks(4)(semesters, privateCourses, privateDegrees)
  private val privateGroups = privateStudents.grouped(25).map(s => GroupDb("", takeOneOf(privateLabs).id, s.map(_.id).toSet)).toList

  "A LwmServiceDaoSpec" should {
    "add students to existing/running labwork" in {
      val lapps = privateGroups.flatMap(g => g.members.map(s => LabworkApplicationDb(g.labwork, s, Set.empty)))
      val cards = lapps.flatMap(l => populateReportCardEntries(1, 2, withRescheduledAndRetry = false)
        (privateLabs.filter(_.id == l.labwork), privateStudents.filter(_.id == l.applicant)))

      await(labworkApplicationDao.createMany(lapps))
      await(groupDao.createMany(privateGroups))
      await(reportCardEntryDao.createMany(cards))

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

      val cardsFromDestStudent = await(reportCardEntryDao.get(
        List(ReportCardEntryStudentFilter(group.members.head.toString), ReportCardEntryLabworkFilter(group.labwork.toString)),
        atomic = false
      )).map(_.asInstanceOf[PostgresReportCardEntry]).sortBy(c => c.date.toLocalDateTime(c.start))

      cardsFromDestStudent.size shouldBe reportCards.size
      reportCards.map(_.toLwmModel).sortBy(c => c.date.toLocalDateTime(c.start)).zip(cardsFromDestStudent).foreach {
        case (c, d) =>
          c.student shouldBe student.id
          c.labwork shouldBe labwork.id
          (c.rescheduled.isEmpty && c.retry.isEmpty) shouldBe true
          c.entryTypes.forall(t =>
            t.int == 0 && t.bool.isEmpty &&
              !d.entryTypes.exists(_.id == t.id)
          ) shouldBe true

          c.date.isEqual(d.date) &&
            c.start.withMillisOfSecond(0).isEqual(d.start.withMillisOfSecond(0)) &&
            c.end.withMillisOfSecond(0).isEqual(d.end.withMillisOfSecond(0))shouldBe true

          c.id should not be d.id
      }
    }
  }

  override protected val dependencies = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(privateDegrees),
    TableQuery[UserTable].forceInsertAll(employees ++ privateStudents),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(privateCourses),
    TableQuery[LabworkTable].forceInsertAll(privateLabs),
    TableQuery[RoomTable].forceInsertAll(rooms)
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
