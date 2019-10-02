package dao

import java.sql.{Date, Time}
import java.util.UUID

import database._
import models._
import org.joda.time.LocalDate
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write

final class AbstractReportCardEntryDaoSpec extends AbstractExpandableDaoSpec[ReportCardEntryTable, ReportCardEntryDb, ReportCardEntryLike] {

  import AbstractDaoSpec._
  import slick.jdbc.PostgresProfile.api._
  import utils.date.DateTimeOps._

  import scala.concurrent.ExecutionContext.Implicits.global

  private lazy val privateLabs = populateLabworks(20)(semesters, courses, degrees)
  private lazy val privateStudents = populateStudents(100)(degrees)

  def reportCardEntryAtom(entry: ReportCardEntryDb)(labworks: List[LabworkDb], students: List[UserDb], rooms: List[RoomDb]) = ReportCardEntryAtom(
    students.find(_.id == entry.student).get.toUniqueEntity,
    labworks.find(_.id == entry.labwork).get.toUniqueEntity,
    entry.label,
    entry.date.localDate,
    entry.start.localTime,
    entry.end.localTime,
    rooms.find(_.id == entry.room).get.toUniqueEntity,
    entry.entryTypes.map(_.toUniqueEntity),
    entry.assignmentIndex,
    entry.rescheduled.map(r =>
      ReportCardRescheduledAtom(r.date.localDate, r.start.localTime, r.end.localTime, rooms.find(_.id == r.room).get.toUniqueEntity, r.reason, r.id)
    ),
    entry.retry.map(r =>
      ReportCardRetryAtom(r.date.localDate, r.start.localTime, r.end.localTime, rooms.find(_.id == r.room).get.toUniqueEntity, r.entryTypes.map(_.toUniqueEntity), r.reason, r.id)
    ),
    entry.id
  )

  override protected def name: String = "reportCardEntry"

  override protected val dbEntity: ReportCardEntryDb =
    ReportCardEntryDb(students.head.id, labworks.head.id, "label", Date.valueOf("1990-05-02"), Time.valueOf("17:00:00"), Time.valueOf("18:00:00"), rooms.head.id, Set.empty, 1)

  override protected val invalidDuplicateOfDbEntity: ReportCardEntryDb =
    dbEntity.copy(date = LocalDate.now.minusDays(1).sqlDate, id = UUID.randomUUID)

  override protected val invalidUpdateOfDbEntity: ReportCardEntryDb =
    dbEntity.copy(assignmentIndex = dbEntity.assignmentIndex + 1)

  override protected val validUpdateOnDbEntity: ReportCardEntryDb =
    dbEntity.copy(date = Date.valueOf("1990-06-02"), start = Time.valueOf("16:00:00"))

  override protected val dbEntities: List[ReportCardEntryDb] = reportCardEntries

  override protected val lwmAtom: ReportCardEntryLike = reportCardEntryAtom(dbEntity)(labworks, students, rooms)

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees ++ students ++ privateStudents),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks ++ privateLabs),
    TableQuery[RoomTable].forceInsertAll(rooms)
  )

  override protected val toAdd: List[ReportCardEntryDb] = populateReportCardEntries(10, 8, withRescheduledAndRetry = true)(privateLabs, privateStudents)

  override protected val numberOfUpdates: Int = 10

  override protected val numberOfDeletions: Int = 20

  override protected def update(toUpdate: List[ReportCardEntryDb]): List[ReportCardEntryDb] = {
    toUpdate.map(r => r.copy(label = "updated", date = r.date.localDate.plusDays(1).sqlDate, room = takeOneOf(rooms).id))
  }

  override protected def atom(dbModel: ReportCardEntryDb): ReportCardEntryLike = reportCardEntryAtom(dbModel)(privateLabs, privateStudents, rooms)

  override protected def expanderSpecs(dbModel: ReportCardEntryDb, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read] = DBIO.seq(
    dao.entryTypeQuery.filter(_.reportCardEntry === dbModel.id).result.map { entryTypes =>
      entryTypes.toSet shouldBe (if (isDefined) dbModel.entryTypes else Set.empty)
    },
    dao.rescheduledQuery.filter(_.reportCardEntry === dbModel.id).result.map { rescheduled =>
      rescheduled.toSet shouldBe (if (isDefined) dbModel.rescheduled.toSet else Set.empty)
    },
    dao.retryQuery.filter(_.reportCardEntry === dbModel.id).joinLeft(dao.entryTypeQuery).on(_.id === _.reportCardRetry).result.map(_.groupBy(_._1.id)).map(_.foreach {
      case (_, values) => values.map(_._1).head.copy(entryTypes = values.flatMap(_._2).toSet) shouldBe dbModel.retry.get
    })
  )

  override protected val dao: ReportCardEntryDao = app.injector.instanceOf(classOf[ReportCardEntryDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
