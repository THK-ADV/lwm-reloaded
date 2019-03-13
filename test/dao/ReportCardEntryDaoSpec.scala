package dao

import java.sql.{Date, Time}

import database._
import models._
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write

final class ReportCardEntryDaoSpec extends AbstractExpandableDaoSpec[ReportCardEntryTable, ReportCardEntryDb, ReportCardEntryLike] {

  import AbstractDaoSpec._
  import slick.jdbc.PostgresProfile.api._
  import utils.LwmDateTime._

  import scala.concurrent.ExecutionContext.Implicits.global

  private lazy val privateLabs = populateLabworks(20)(semesters, courses, degrees)
  private lazy val privateStudents = populateStudents(100)

  "A ReportCardEntryDaoSpec also" should {
    "filter by scheduleEntry while considering rescheduled and retry entries" in { // TODO this makes no sense
      /*val semester = populateSemester(1).head
      val degree = populateDegrees(1).head
      val course = populateCourses(1)(n => n).head
      val labworks = List(
        LabworkDb("A", "A", semester.id, course.id, degree.id)
        //LabworkDb("B", "B", semester.id, course.id, degree.id),
      )
      val rooms = List(RoomDb("A", "A"), RoomDb("B", "B"))

      val groups = List(
        GroupDb("A", labworks(0).id, privateStudents.take(20).map(_.id).toSet),
        GroupDb("B", labworks(0).id, privateStudents.slice(20, 40).map(_.id).toSet)
      )

      val scheduleEntries = List(
        ScheduleEntryDb(labworks(0).id, LocalTime.now.sqlTime, LocalTime.now.plusHours(1).sqlTime, LocalDate.now.sqlDate, rooms(0).id, Set.empty, groups(0).id),
        ScheduleEntryDb(labworks(0).id, LocalTime.now.sqlTime, LocalTime.now.plusHours(1).sqlTime, LocalDate.now.sqlDate, rooms(1).id, Set.empty, groups(1).id)
      )
      val schedule = ScheduleGen(labworks(0).id, scheduleEntries.map(e => ScheduleEntryGen(e.start.localTime, e.end.localTime, e.date.localDate, e.room, e.supervisor, groups.find(_.id == e.group).get.toLwmModel))

      services.ReportCardService.reportCards(schedule, )

      run(DBIO.seq(
        TableQuery[SemesterTable].forceInsert(semester),
        TableQuery[DegreeTable].forceInsert(degree),
        TableQuery[CourseTable].forceInsert(course),
        TableQuery[LabworkTable].forceInsertAll(labworks),
        TableQuery[RoomTable].forceInsertAll(rooms)
      ))

      await(get(List(ReportCardEntryScheduleEntryFilter("")), atomic = false))*/
    }
  }

  def reportCardEntryAtom(entry: ReportCardEntryDb)(labworks: List[LabworkDb], students: List[UserDb], rooms: List[RoomDb]) = ReportCardEntryAtom(
    students.find(_.id == entry.student).get.toUniqueEntity,
    labworks.find(_.id == entry.labwork).get.toUniqueEntity,
    entry.label,
    entry.date.localDate,
    entry.start.localTime,
    entry.end.localTime,
    rooms.find(_.id == entry.room).get.toUniqueEntity,
    entry.entryTypes.map(_.toUniqueEntity),
    entry.rescheduled.map(r =>
      ReportCardRescheduledAtom(r.date.localDate, r.start.localTime, r.end.localTime, rooms.find(_.id == r.room).get.toUniqueEntity, r.reason, r.id)
    ),
    entry.retry.map(r =>
      ReportCardRetryAtom(r.date.localDate, r.start.localTime, r.end.localTime, rooms.find(_.id == r.room).get.toUniqueEntity, r.entryTypes.map(_.toUniqueEntity), r.reason, r.id)
    ),
    entry.id
  )

  override protected def name: String = "reportCardEntry"

  override protected val dbEntity: ReportCardEntryDb = ReportCardEntryDb(students.head.id, labworks.head.id, "label", Date.valueOf("1990-05-02"), Time.valueOf("17:00:00"), Time.valueOf("18:00:00"), rooms.head.id, Set.empty)

  override protected val invalidDuplicateOfDbEntity: ReportCardEntryDb = dbEntity

  override protected val invalidUpdateOfDbEntity: ReportCardEntryDb = dbEntity

  override protected val validUpdateOnDbEntity: ReportCardEntryDb = dbEntity.copy(date = Date.valueOf("1990-06-02"), start = Time.valueOf("16:00:00"))

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

  override protected val toAdd: List[ReportCardEntryDb] = populateReportCardEntries(100, 8, withRescheduledAndRetry = true)(privateLabs, privateStudents)

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
