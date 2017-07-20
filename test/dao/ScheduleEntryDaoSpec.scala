package dao

import java.util.UUID

import models._
import services.{AbstractExpandableDaoSpec, ScheduleEntryDao}
import slick.driver.PostgresDriver.api._
import slick.lifted.TableQuery
import store._
import models.LwmDateTime._

final class ScheduleEntryDaoSpec extends AbstractExpandableDaoSpec[ScheduleEntryTable, ScheduleEntryDb, ScheduleEntry] with ScheduleEntryDao {
  import services.AbstractDaoSpec._

  private lazy val privateSupervisors = populateEmployees(50)
  private lazy val privateLabs = populateLabworks(10)(semesters, courses, degrees)

  // TODO equals for scheduleEntry
  "A ScheduleEntryDaoSpec also" should {
    "return competitive schedules based on given labwork" in {
      val degrees = populateDegrees(4)
      val semesters = populateSemester(4)
      val courses = populateCourses(degrees.size * 4)(_ % 4)
      val labworks = populateLabworks(courses.size * degrees.size)(semesters, courses, degrees)

      val entries = labworks.tail.flatMap { lab =>
        populateScheduleEntry(8 * 8)(List(lab), rooms, employees, groups)
      }

      await(transaction(
        scheduleEntrySupervisorQuery.delete,
        tableQuery.delete,
        TableQuery[DegreeTable].forceInsertAll(degrees),
        TableQuery[SemesterTable].forceInsertAll(semesters),
        TableQuery[CourseTable].forceInsertAll(courses),
        TableQuery[LabworkTable].forceInsertAll(labworks),
        tableQuery.forceInsertAll(entries)
      ))

      val current = {
        val l = labworks.head
        val course = courses.find(_.id == l.course).map { c =>
          PostgresCourseAtom(c.label, c.description, c.abbreviation, employees.find(_.id == c.lecturer).get.toLwmModel, c.semesterIndex, c.id)
        }
        PostgresLabworkAtom(l.label, l.description, semesters.find(_.id == l.semester).get.toLwmModel, course.get, degrees.find(_.id == l.degree).get.toLwmModel, l.subscribable, l.published, l.id)
      }

      val result = await(competitive(current))

      run(DBIO.seq(
        tableQuery.filter { s =>
          s.labworkFk.flatMap(_.courseFk).filter(_.semesterIndex === current.course.semesterIndex).exists &&
            s.labworkFk.flatMap(_.semesterFk).filter(_.id === current.semester.id).exists &&
            s.labworkFk.flatMap(_.degreeFk).filter(_.id === current.degree.id).exists &&
            s.labwork =!= current.id
        }.result.map { res =>
          res.size shouldBe result.flatMap(_.entries).size

          res.groupBy(_.labwork).foreach {
            case (l, e) =>
              result.count(_.labwork == l) shouldBe 1

              result.find(_.labwork == l).get.entries.forall { g =>
                e.count { se =>
                  se.group == g.group.id &&
                    se.date.localDate.isEqual(g.date) &&
                    se.start.localTime.isEqual(g.start) &&
                    se.end.localTime.isEqual(g.end) &&
                    se.room == g.room
                } == 1
              } shouldBe true
          }
        }
      ))
    }
  }

  override protected def name: String = "scheduleEntry"

  override protected val dbEntity: ScheduleEntryDb = populateScheduleEntry(1)(labworks, rooms, employees, groups).head.copy(supervisor = Set.empty)

  override protected val invalidDuplicateOfDbEntity: ScheduleEntryDb = dbEntity.copy(room = randomRoom.id)

  override protected val invalidUpdateOfDbEntity: ScheduleEntryDb = dbEntity.copy(labwork = randomLabwork.id, group = randomGroup.id)

  override protected val validUpdateOnDbEntity: ScheduleEntryDb = dbEntity.copy(room = randomRoom.id)

  override protected val dbEntities: List[ScheduleEntryDb] = scheduleEntries

  override protected val lwmEntity: ScheduleEntry = dbEntity.toLwmModel

  override protected val lwmAtom: ScheduleEntry = atom(dbEntity)

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees ++ students ++ privateSupervisors),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks ++ privateLabs),
    TableQuery[RoomTable].forceInsertAll(rooms),
    TableQuery[GroupTable].forceInsertAll(groups),
    TableQuery[GroupMembershipTable].forceInsertAll(groupMemberships)
  )

  override protected val toAdd: List[ScheduleEntryDb] = privateLabs.flatMap { lab =>
    populateScheduleEntry(8)(List(lab), rooms, privateSupervisors, groups)
  }

  override protected val numberOfUpdates: Int = 10

  override protected val numberOfDeletions: Int = 10

  override protected def update(toUpdate: List[ScheduleEntryDb]): List[ScheduleEntryDb] = toUpdate.map { e =>
    e.copy(room = randomRoom.id, supervisor = takeSomeOf(privateSupervisors).map(_.id).toSet)
  }

  override protected def atom(dbModel: ScheduleEntryDb): ScheduleEntry = {
    val labwork = for {
      l <- (privateLabs ++ labworks).find(_.id == dbModel.labwork)
      s <- semesters.find(_.id == l.semester)
      d <- degrees.find(_.id == l.degree)
      c <- courses.find(_.id == l.course)
      lec <- employees.find(_.id == c.lecturer)
      ca = PostgresCourseAtom(c.label, c.description, c.abbreviation, lec.toLwmModel, c.semesterIndex, c.id)
    } yield PostgresLabworkAtom(l.label, l.description, s.toLwmModel, ca, d.toLwmModel, l.subscribable, l.published, l.id)

    PostgresScheduleEntryAtom(
      labwork.get,
      dbModel.start.localTime,
      dbModel.end.localTime,
      dbModel.date.localDate,
      rooms.find(_.id == dbModel.room).get.toLwmModel,
      (privateSupervisors ++ employees).filter(u => dbModel.supervisor.contains(u.id)).map(_.toLwmModel).toSet,
      groups.find(_.id == dbModel.group).get.toLwmModel,
      dbModel.id
    )
  }

  override protected def expanderSpecs(dbModel: ScheduleEntryDb, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read] = DBIO.seq(
    scheduleEntrySupervisorQuery.filter(_.scheduleEntry === dbModel.id).result.map { sups =>
      sups.map(_.supervisor).toSet shouldBe (if (isDefined) dbModel.supervisor else Set.empty)
    }
  )
}
