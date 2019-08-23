package dao

import java.util.UUID

import base.{DateGenerator, PostgresDbSpec}
import database._
import database.helper.LdapUserStatus.StudentStatus
import play.api.inject.guice.GuiceableModule
import slick.dbio.{DBIOAction, Effect, NoStream}
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.{LocalDateConverter, LocalTimeConverter, localDateOrd}

class LwmServiceDaoSpec extends PostgresDbSpec with DateGenerator {

  import scala.concurrent.ExecutionContext.Implicits.global

  val dao = app.injector.instanceOf(classOf[LwmServiceDao])
  val groupDap = app.injector.instanceOf(classOf[GroupDao])
  val reportCardEntryDao = app.injector.instanceOf(classOf[ReportCardEntryDao])
  val labworkApplicationDao = app.injector.instanceOf(classOf[LabworkApplicationDao])

  private val rooms = AbstractDaoSpec.rooms
  private val degrees = AbstractDaoSpec.populateDegrees(2)
  private val students = AbstractDaoSpec.populateStudents(100)(degrees)
  private val employees = AbstractDaoSpec.employees
  private val users = employees ++ students
  private val semesters = AbstractDaoSpec.populateSemester(1)
  private val courses = AbstractDaoSpec.populateCourses(1)(_ => 1)
  private val labworks = AbstractDaoSpec.populateLabworks(3)(semesters, courses, degrees)
  private val applications = AbstractDaoSpec.populateLabworkApplications(students.size, true)(labworks, students)

  override protected def beforeEach(): Unit = {
    super.beforeEach()

    val clear = for {
      gs <- groupDap.get()
      _ <- groupDap.deleteMany(gs.map(_.id).toList)
      crds <- reportCardEntryDao.get()
      _ <- reportCardEntryDao.deleteMany(crds.map(_.id).toList)
    } yield Unit


    async(clear)(_ => Unit)
  }

  "A LwmServiceDao" should {

    "add a student into an existing group even if an application exists" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      val groups = createGroups
      val reportCardEntries = createReportCardEntries(groups).filterNot(_.labwork == labwork.id)
      val application = LabworkApplicationDb(labwork.id, student.id, Set.empty)

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)
      async(reportCardEntryDao.createMany(reportCardEntries))(_ => Unit)
      async(labworkApplicationDao.create(application))(_ => Unit)

      val destGrp = groups.find(_.labwork == labwork.id).get

      async(dao.addStudentToGroup(student.id, labwork.id, destGrp.id)) {
        case (lapp, membership, srcStudent, cards) =>
          lapp shouldBe application

          membership.student shouldBe student.id
          membership.group shouldBe destGrp.id

          srcStudent.isDefined shouldBe true
          cards shouldBe empty
      }
    }

    "add a student into an existing group by creating a new application" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      val added = GroupDb("foo", labworks.find(_.id != labwork.id).get.id, Set(student.id))
      val groups = createGroups :+ added
      val reportCardEntries = createReportCardEntries(groups).filterNot(_.labwork == labwork.id)

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)
      async(reportCardEntryDao.createMany(reportCardEntries))(_ => Unit)

      val destGrp = groups.find(_.labwork == labwork.id).get

      async(dao.addStudentToGroup(student.id, labwork.id, destGrp.id)) {
        case (lapp, membership, srcStudent, cards) =>
          lapp.labwork shouldBe labwork.id
          lapp.applicant shouldBe student.id
          lapp.friends shouldBe empty

          membership.student shouldBe student.id
          membership.group shouldBe destGrp.id

          srcStudent.isDefined shouldBe true
          cards shouldBe empty
      }
    }

    "add a student into an existing group by creating a new application and coping reportCardEntries" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      val groups = createGroups
      val reportCardEntries = createReportCardEntries(groups)

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)
      async(reportCardEntryDao.createMany(reportCardEntries))(_ => Unit)

      val destGrp = groups.find(_.labwork == labwork.id).get

      async(dao.addStudentToGroup(student.id, labwork.id, destGrp.id)) {
        case (lapp, membership, srcStudent, cards) =>
          lapp.labwork shouldBe labwork.id
          lapp.applicant shouldBe student.id
          lapp.friends shouldBe empty

          membership.student shouldBe student.id
          membership.group shouldBe destGrp.id

          val lhs = cards
            .map(_.toUniqueEntity)
            .sortBy(_.date)

          lhs.forall(r => r.labwork == labwork.id && r.student == student.id && r.rescheduled.isEmpty && r.retry.isEmpty && r.entryTypes.forall(_.bool.isEmpty)) shouldBe true

          val rhs = reportCardEntries
            .filter(_.student == srcStudent.get)
            .map(_.toUniqueEntity)
            .sortBy(_.date)

          lhs.size shouldBe rhs.size
          lhs.zip(rhs).foreach {
            case (l, r) =>
              l.student should not be r.student
              l.id should not be r.id

              l.labwork shouldBe r.labwork
              l.date shouldBe r.date
              l.start shouldBe r.start
              l.end shouldBe r.end
              l.room shouldBe r.room
          }
      }
    }

    "fail adding a student into an existing group if he has already one" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      val groups = createGroups
      val destGrp = groups.find(_.labwork == labwork.id).get
      val added = destGrp.copy(members = destGrp.members + student.id)

      val reportCardEntries = createReportCardEntries(groups).filterNot(_.labwork == labwork.id)

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)
      async(reportCardEntryDao.createMany(reportCardEntries))(_ => Unit)
      async(groupDap.update(added))(_ => Unit)

      async(dao.addStudentToGroup(student.id, labwork.id, destGrp.id).failed)(_.getMessage shouldBe s"student ${student.id} is already in group ${destGrp.id}")
    }
  }

  private def createReportCardEntries(groups: List[GroupDb]) = {
    val reportCardEntries = for {
      g <- groups
      s <- g.members
      e <- (0 until 4).map { i =>
        val id = UUID.randomUUID
        val types = (0 until 4).map(i => ReportCardEntryTypeDb(Some(id), None, i.toString)).toSet

        ReportCardEntryDb(s, g.labwork, i.toString, randomLocalDate.sqlDate, randomLocalTime.sqlTime, randomLocalTime.sqlTime, rooms.head.id, types, id = id)
      }
    } yield e
    reportCardEntries
  }

  private def createGroups = {
    students.grouped(labworks.size).zip(labworks.toIterator).zipWithIndex.map {
      case ((s, l), i) => GroupDb(i.toString, l.id, s.map(_.id).toSet)
    }.toList
  }

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  override protected def dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[RoomTable].forceInsertAll(rooms),
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(users),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks),
    TableQuery[LabworkApplicationTable].forceInsertAll(applications)
  )
}
