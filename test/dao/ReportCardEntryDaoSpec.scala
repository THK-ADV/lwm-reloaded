package dao

import java.util.UUID

import models._
import services.{AbstractDaoSpec, ReportCardEntryDao}
import slick.dbio.Effect.Write
import store._

final class ReportCardEntryDaoSpec extends AbstractDaoSpec[ReportCardEntryTable, ReportCardEntryDb, ReportCardEntry] with ReportCardEntryDao {
  import services.AbstractDaoSpec._
  import slick.driver.PostgresDriver.api._
  import models.LwmDateTime._

  private lazy val privateLabs = populateLabworks(20)
  private lazy val privateStudents = populateStudents(100)

  def atom(entry: ReportCardEntryDb)(labworks: List[LabworkDb], students: List[DbUser], rooms: List[RoomDb]) = PostgresReportCardEntryAtom(
    students.find(_.id == entry.student).get.toUser,
    labworks.find(_.id == entry.labwork).get.toLabwork,
    entry.label,
    entry.date.localDate,
    entry.start.localTime,
    entry.end.localTime,
    rooms.find(_.id == entry.room).get.toRoom,
    entry.entryTypes.map(_.toReportCardEntryType),
    entry.rescheduled.map(r =>
      PostgresReportCardRescheduledAtom(r.date.localDate, r.start.localTime, r.end.localTime, rooms.find(_.id == r.room).get.toRoom, r.reason, r.id)
    ),
    entry.retry.map(r =>
      PostgresReportCardRetryAtom(r.date.localDate, r.start.localTime, r.end.localTime, rooms.find(_.id == r.room).get.toRoom, r.entryTypes.map(_.toReportCardEntryType), r.reason, r.id)
    ),
    entry.id
  )

  "A ReportCardEntryDaoSpec also" should {
    // TODO maybe we can refactor expandable testing into dedicated class

    val toAdd = populateReportCardEntries(100, 8, withRescheduledAndRetry = true)(privateLabs, privateStudents)
    val toUpdate = toAdd.take(10)
    val toDelete = toAdd.slice(toUpdate.size, toUpdate.size + 20)
    val remainingAfterDelete = toAdd.drop(toUpdate.size + toDelete.size)

    def assertEverythingOf(reportCards: List[ReportCardEntryDb]): Unit = {
      val isDefined = reportCards.forall(_.invalidated.isEmpty)
      val ids = reportCards.map(_.id)
      val cs = reportCards.map(_.toReportCardEntry)
      val csa = reportCards.map(r => atom(r)(privateLabs, privateStudents, rooms))

      zipAndCompare(await(getMany(ids, atomic = false)), cs) shouldBe isDefined
      zipAndCompare(await(getMany(ids)), csa) shouldBe isDefined

      reportCards foreach { c =>
        run(DBIO.seq(
          entryTypeQuery.filter(_.reportCardEntry === c.id).result.map { entryTypes =>
            entryTypes.toSet shouldBe (if (isDefined) c.entryTypes else Set.empty)
          }
        ).andThen(
          rescheduledQuery.filter(_.reportCardEntry === c.id).result.map { rescheduled =>
            rescheduled.toSet shouldBe (if (isDefined) c.rescheduled.toSet else Set.empty)
          }
        ).andThen(
          retryQuery.filter(_.reportCardEntry === c.id).joinLeft(entryTypeQuery).on(_.id === _.reportCardRetry).result.map(_.groupBy(_._1.id)).map(_.foreach {
            case (_, values) => values.map(_._1).head.copy(entryTypes = values.flatMap(_._2).toSet) shouldBe c.retry.get
          })
        ))
      }
    }

    "create reportCardEntries with entryTypes, reportCard-rescheduled and reportCard-retry" in {
      await(createMany(toAdd)) shouldBe toAdd
      assertEverythingOf(toAdd)
    }

    "update few reportCardEntries with entryTypes, reportCard-rescheduled and reportCard-retry" in {
      val updated = toUpdate.map(r => r.copy(label = "updated", date = r.date.localDate.plusDays(1).sqlDate, room = takeOneOf(rooms).id))

      await(updateMany(updated)) shouldBe updated.map(Some(_))
      assertEverythingOf(updated)
    }

    "delete few reportCardEntries with entryTypes, reportCard-rescheduled and reportCard-retry" in {
      val deleted = await(deleteManyEntities(toDelete)).flatten

      deleted.count(_.invalidated.isDefined) shouldBe toDelete.size

      val remainingIds = remainingAfterDelete.map(_.id)
      zipAndCompare(await(getMany(remainingIds, atomic = false)), remainingAfterDelete.map(_.toReportCardEntry)) shouldBe true
      zipAndCompare(await(getMany(remainingIds)), remainingAfterDelete.map(r => atom(r)(privateLabs, privateStudents, rooms))) shouldBe true

      assertEverythingOf(deleted)
    }
  }

  override protected def name: String = "reportCardEntry"

  override protected val dbEntity: ReportCardEntryDb = populateReportCardEntries(1, 8, withRescheduledAndRetry = false)(labworks, students).head.copy(entryTypes = Set.empty) // need to be empty because basic tests don't expand

  override protected val invalidDuplicateOfDbEntity: ReportCardEntryDb = dbEntity.copy(id = UUID.randomUUID)

  override protected val invalidUpdateOfDbEntity: ReportCardEntryDb = dbEntity.copy(student = students.find(_.id != dbEntity.student).get.id)

  override protected val validUpdateOnDbEntity: ReportCardEntryDb = dbEntity.copy(label = "updated label")

  override protected val dbEntities: List[ReportCardEntryDb] = reportCardEntries

  override protected val lwmEntity: ReportCardEntry = dbEntity.toReportCardEntry(dbEntity.entryTypes.toSeq, None, None)

  override protected val lwmAtom: ReportCardEntry = atom(dbEntity)(labworks, students, rooms)

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees ++ students ++ privateStudents),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks ++ privateLabs),
    TableQuery[RoomTable].forceInsertAll(rooms)
  )
}
