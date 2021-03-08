package dao

import java.util.UUID

import base.{DateGenerator, PostgresDbSpec}
import dao.helper.NoEntityFound
import dao.helper.TableFilter.{labworkFilter, userFilter}
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
  private val courses = AbstractDaoSpec.populateCourses(1)(employees)(_ => 1)
  private val labworks = AbstractDaoSpec.populateLabworks(3)(semesters, courses, degrees)
  private val applications = AbstractDaoSpec.populateLabworkApplications(students.size, withFriends = true)(labworks, students)

  override protected def beforeEach(): Unit = {
    super.beforeEach()

    val clear = for {
      gs <- groupDap.get()
      _ <- groupDap.invalidateMany(gs.map(_.id).toList)
      crds <- reportCardEntryDao.get()
      _ <- reportCardEntryDao.invalidateMany(crds.map(_.id).toList)
    } yield Unit


    async(clear)(_ => Unit)
  }

  "A LwmServiceDao" should {

    "insert a student into an existing group even if an application exists" in {
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

      async(dao.insertStudentToGroup(student.id, labwork.id, destGrp.id)) {
        case (membership, lapp, cards, srcStudent) =>
          lapp shouldBe application.toUniqueEntity

          membership.student shouldBe student.id
          membership.group shouldBe destGrp.id

          srcStudent.isDefined shouldBe true
          cards shouldBe empty
      }
    }

    "insert a student into an existing group by creating a new application" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      val added = GroupDb("foo", labworks.find(_.id != labwork.id).get.id, Set(student.id))
      val groups = createGroups :+ added
      val reportCardEntries = createReportCardEntries(groups).filterNot(_.labwork == labwork.id)

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)
      async(reportCardEntryDao.createMany(reportCardEntries))(_ => Unit)

      val destGrp = groups.find(_.labwork == labwork.id).get

      async(dao.insertStudentToGroup(student.id, labwork.id, destGrp.id)) {
        case (membership, lapp, cards, srcStudent) =>
          lapp.labwork shouldBe labwork.id
          lapp.applicant shouldBe student.id
          lapp.friends shouldBe empty

          membership.student shouldBe student.id
          membership.group shouldBe destGrp.id

          srcStudent.isDefined shouldBe true
          cards shouldBe empty
      }
    }

    "insert a student into an existing group by creating a new application and copying reportCardEntries" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      val groups = createGroups
      val reportCardEntries = createReportCardEntries(groups)

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)
      async(reportCardEntryDao.createMany(reportCardEntries))(_ => Unit)

      val destGrp = groups.find(_.labwork == labwork.id).get

      async(dao.insertStudentToGroup(student.id, labwork.id, destGrp.id)) {
        case (membership, lapp, cards, srcStudent) =>
          lapp.labwork shouldBe labwork.id
          lapp.applicant shouldBe student.id
          lapp.friends shouldBe empty

          membership.student shouldBe student.id
          membership.group shouldBe destGrp.id

          val lhs = cards
            .sortBy(_.date)

          lhs.forall(r => r.labwork == labwork.id && r.student == student.id && r.rescheduled.isEmpty && r.entryTypes.forall(_.bool.isEmpty)) shouldBe true

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
              l.entryTypes.size shouldBe r.entryTypes.size
              l.entryTypes.map(_.entryType) shouldBe r.entryTypes.map(_.entryType)
              l.entryTypes.forall(t => t.bool.isEmpty && t.int == 0) shouldBe true
          }
      }
    }

    "fail inserting a student into an existing group if he has already one" in {
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

      async(dao.insertStudentToGroup(student.id, labwork.id, destGrp.id).failed) { error =>
        error.getMessage shouldBe s"student ${student.id} is already in group ${destGrp.id}"
      }
    }

    "remove a student from an existing group by removing application, membership and reportCardEntries" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      var groups = createGroups
      var group = groups.find(_.labwork == labwork.id).get
      group = group.copy(members = group.members + student.id)
      groups = groups.map(g => if (g.id == group.id) group else g)

      val reportCardEntries = createReportCardEntries(groups)
      val application = LabworkApplicationDb(labwork.id, student.id, Set.empty)

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)
      async(reportCardEntryDao.createMany(reportCardEntries))(_ => Unit)
      async(labworkApplicationDao.create(application))(_ => Unit)

      async(dao.removeStudentFromGroup(student.id, labwork.id, group.id)) {
        case (deletedGrp, deletedLapp, deletedCards) =>
          deletedGrp shouldBe true

          deletedLapp.applicant shouldBe student.id
          deletedLapp.labwork shouldBe labwork.id

          deletedCards should not be empty
          deletedCards.forall(c => c.student == student.id && c.labwork == labwork.id) shouldBe true
      }

      async(labworkApplicationDao.getSingle(application.id))(_.isEmpty shouldBe true)
      runAsync(groupDap.isInGroup(student.id, group.id))(_ shouldBe false)
      async(reportCardEntryDao.get(List(labworkFilter(labwork.id), userFilter(student.id))))(_.isEmpty shouldBe true)
    }

    "remove a student from an existing group even if no reportCardEntries exists" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      var groups = createGroups
      var group = groups.find(_.labwork == labwork.id).get
      group = group.copy(members = group.members + student.id)
      groups = groups.map(g => if (g.id == group.id) group else g)

      val reportCardEntries = createReportCardEntries(groups).filterNot(_.labwork == labwork.id)
      val application = LabworkApplicationDb(labwork.id, student.id, Set.empty)

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)
      async(reportCardEntryDao.createMany(reportCardEntries))(_ => Unit)
      async(labworkApplicationDao.create(application))(_ => Unit)

      async(dao.removeStudentFromGroup(student.id, labwork.id, group.id)) {
        case (deletedGrp, deletedLapp, deletedCards) =>
          deletedGrp shouldBe true

          deletedLapp.applicant shouldBe student.id
          deletedLapp.labwork shouldBe labwork.id

          deletedCards shouldBe empty
      }

      async(labworkApplicationDao.getSingle(application.id))(_.isEmpty shouldBe true)
      runAsync(groupDap.isInGroup(student.id, group.id))(_ shouldBe false)
      async(reportCardEntryDao.get(List(labworkFilter(labwork.id), userFilter(student.id))))(_.isEmpty shouldBe true)
    }

    "fail removing a student from a group if he is no member of that group" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      val groups = createGroups
      val group = groups.find(g => g.labwork == labwork.id && !g.members.contains(student.id)).get

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)

      async(dao.removeStudentFromGroup(student.id, labwork.id, group.id).failed) { error =>
        error.getMessage shouldBe s"student ${student.id} is not a member of group ${group.id}"
      }
    }

    "fail removing a student from an existing group if he has no valid application" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      var groups = createGroups
      var group = groups.find(_.labwork == labwork.id).get
      group = group.copy(members = group.members + student.id)
      groups = groups.map(g => if (g.id == group.id) group else g)

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)

      async(dao.removeStudentFromGroup(student.id, labwork.id, group.id).failed) { error =>
        error.getMessage shouldBe NoEntityFound.getMessage
      }
    }

    "fail removing a student from an existing group if he would be the last one" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      var groups = createGroups
      var group = groups.find(_.labwork == labwork.id).get
      group = group.copy(members = Set(student.id))
      groups = groups.map(g => if (g.id == group.id) group else g)

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)

      async(dao.removeStudentFromGroup(student.id, labwork.id, group.id).failed) { error =>
        error.getMessage shouldBe "there must be at least one member left after removal"
      }
    }

    "move a student into another group by changing the membership and copying reportCardEntries" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      var groups = createGroups
      var srcGroup = groups.find(_.labwork == labwork.id).get
      srcGroup = srcGroup.copy(members = srcGroup.members + student.id)
      groups = groups.map(g => if (g.id == srcGroup.id) srcGroup else g)

      val destGroup = groups.find(g => g.id != srcGroup.id && g.labwork == labwork.id).get
      val reportCardEntries = createReportCardEntries(groups).map(e => if (e.student == student.id && e.labwork == labwork.id) e.copy(entryTypes = e.entryTypes.map(_.copy(bool = Some(true)))) else e)

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)
      async(reportCardEntryDao.createMany(reportCardEntries))(_ => Unit)

      async(dao.moveStudentToGroup(student.id, labwork.id, srcGroup.id, destGroup.id)) {
        case (deletedGrp, newMembership, srcCards, destCards, updatedCards) =>
          deletedGrp shouldBe true

          newMembership.student shouldBe student.id
          newMembership.group shouldBe destGroup.id

          updatedCards should not be empty

          srcCards.zip(destCards).zip(updatedCards).foreach {
            case ((s, d), u) =>
              s.id shouldBe u.id
              d.id should not be u.id

              u.student shouldBe s.student
              u.labwork shouldBe s.labwork
              u.entryTypes shouldBe s.entryTypes
              u.rescheduled shouldBe s.rescheduled

              u.date shouldBe d.date
              u.start shouldBe d.start
              u.end shouldBe d.end
              u.room shouldBe d.room
          }
      }
    }

    "fail moving a student into another group if src- and dest- reportCardEntries are inconsistent" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      var groups = createGroups
      var srcGroup = groups.find(_.labwork == labwork.id).get
      srcGroup = srcGroup.copy(members = srcGroup.members + student.id)
      groups = groups.map(g => if (g.id == srcGroup.id) srcGroup else g)

      val destGroup = groups.find(g => g.id != srcGroup.id && g.labwork == labwork.id).get
      val reportCardEntries = createReportCardEntries(groups).filterNot(e => e.labwork == labwork.id && e.student == student.id)

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)
      async(reportCardEntryDao.createMany(reportCardEntries))(_ => Unit)

      async(dao.moveStudentToGroup(student.id, labwork.id, srcGroup.id, destGroup.id).failed) { error =>
        error.getMessage shouldBe "both src- and dest- reportCardEntries must have the same size"
      }
    }

    "fail moving a student into another group if he has no group in the first place" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      var groups = createGroups
      var srcGroup = groups.find(_.labwork == labwork.id).get
      srcGroup = srcGroup.copy(members = srcGroup.members + student.id)
      groups = groups.map(g => if (g.id == srcGroup.id) srcGroup else g)

      val destGroup = groups.find(g => g.id != srcGroup.id && g.labwork == labwork.id).get
      val reportCardEntries = createReportCardEntries(groups)

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)
      async(reportCardEntryDao.createMany(reportCardEntries))(_ => Unit)

      async(dao.moveStudentToGroup(student.id, labwork.id, destGroup.id, srcGroup.id).failed) { error =>
        error.getMessage shouldBe s"student ${student.id} is not a member of group ${destGroup.id}"
      }
    }

    "fail moving a student into another group if he would be the last one" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      var groups = createGroups
      var srcGroup = groups.find(_.labwork == labwork.id).get
      srcGroup = srcGroup.copy(members = Set(student.id))
      groups = groups.map(g => if (g.id == srcGroup.id) srcGroup else g)

      val destGroup = groups.find(g => g.id != srcGroup.id && g.labwork == labwork.id).get

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)

      async(dao.moveStudentToGroup(student.id, labwork.id, srcGroup.id, destGroup.id).failed) { error =>
        error.getMessage shouldBe "there must be at least one member left after moving"
      }
    }

    "fail moving a student into another group if reportCardEntries are not supported" in {
      val labwork = labworks.head
      val student = UserDb("", "", "", "", StudentStatus, Some(""), Some(labwork.degree))
      var groups = createGroups
      var srcGroup = groups.find(_.labwork == labwork.id).get
      srcGroup = srcGroup.copy(members = srcGroup.members + student.id)
      groups = groups.map(g => if (g.id == srcGroup.id) srcGroup else g)

      val destGroup = groups.find(g => g.id != srcGroup.id && g.labwork == labwork.id).get
      val reportCardEntries = createReportCardEntries(groups).map(e => if (e.assignmentIndex == 1) e.copy(assignmentIndex = -1) else e)

      async(db.run(TableQuery[UserTable].insertOrUpdate(student)))(_ => Unit)
      async(groupDap.createMany(groups))(_ => Unit)
      async(reportCardEntryDao.createMany(reportCardEntries))(_ => Unit)

      async(dao.moveStudentToGroup(student.id, labwork.id, srcGroup.id, destGroup.id).failed) { error =>
        error.getMessage shouldBe "could not copy reportCardEntries because this operation is only supported on cards with a valid assignmentIndex"
      }
    }
  }

  private def createReportCardEntries(groups: List[GroupDb]) = {
    val reportCardEntries = for {
      g <- groups
      s <- g.members
      e <- (1 until 5).map { i =>
        val id = UUID.randomUUID
        val types = (0 until 4).map(i => ReportCardEntryTypeDb(id, i.toString)).toSet

        ReportCardEntryDb(s, g.labwork, i.toString, randomLocalDate.sqlDate, randomLocalTime.sqlTime, randomLocalTime.sqlTime, rooms.head.id, types, i, id = id)
      }
    } yield e
    reportCardEntries
  }

  private def createGroups = {
    students.grouped(labworks.size).grouped(labworks.size).toList.zip(labworks).zipWithIndex.flatMap {
      case ((ss, l), i) => ss.map(s => GroupDb(UUID.randomUUID.toString, l.id, s.map(_.id).toSet))
    }
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
