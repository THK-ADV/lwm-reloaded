package services

import java.util.UUID

import models._
import slick.driver.PostgresDriver
import store.{GroupMembershipTable, GroupTable, ScheduleEntrySupervisorTable, ScheduleEntryTable}
import slick.driver.PostgresDriver.api._
import models.LwmDateTime._

import scala.concurrent.Future

trait ScheduleEntryDao extends AbstractDao[ScheduleEntryTable, ScheduleEntryDb, ScheduleEntry] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery = TableQuery[ScheduleEntryTable]
  protected val scheduleEntrySupervisorQuery: TableQuery[ScheduleEntrySupervisorTable] = TableQuery[ScheduleEntrySupervisorTable]
  protected val groupQuery: TableQuery[GroupTable] = TableQuery[GroupTable]
  protected val groupMembershipQuery: TableQuery[GroupMembershipTable] = TableQuery[GroupMembershipTable]

  override protected def toAtomic(query: Query[ScheduleEntryTable, ScheduleEntryDb, Seq]): Future[Seq[ScheduleEntry]] = ???

  override protected def toUniqueEntity(query: Query[ScheduleEntryTable, ScheduleEntryDb, Seq]): Future[Seq[ScheduleEntry]] = ???

  override protected def existsQuery(entity: ScheduleEntryDb): Query[ScheduleEntryTable, ScheduleEntryDb, Seq] = ???

  override protected def shouldUpdate(existing: ScheduleEntryDb, toUpdate: ScheduleEntryDb): Boolean = ???

  private def collectDependencies(query: Query[ScheduleEntryTable, ScheduleEntryDb, Seq])
                                 (build: ((ScheduleEntryDb, RoomDb, LabworkDb, CourseDb, DegreeDb, SemesterDb, DbUser), (GroupDb, Seq[GroupMembership]), Seq[(ScheduleEntrySupervisor, DbUser)]) => ScheduleEntry) = {
    val mandatory = for {
      q <- query
      r <- q.roomFk
      g <- q.groupFk
      l <- q.labworkFk
      c <- l.courseFk
      d <- l.degreeFk
      s <- l.semesterFk
      lec <- c.lecturerFk
    } yield (q, r, g, l, c, d, s, lec)

    val supervisors = for {
      s <- scheduleEntrySupervisorQuery
      u <- s.supervisorFk
    } yield (s, u)

    val group = groupQuery.joinLeft(groupMembershipQuery).on(_.id === _.group)

    val action = mandatory.joinLeft(supervisors).on(_._1.id === _._1.scheduleEntry).joinLeft(group).on(_._1._1.group === _._1.id).result.map(_.groupBy(_._1._1._1.id).map {
      case (id, dependencies) =>
        val (((se, r, g, l, c, d, s, lec), _), _) = dependencies.find(_._1._1._1.id == id).get
        val sups = dependencies.flatMap(_._1._2)
        val gm = dependencies.flatMap(_._2.flatMap(_._2))

        build((se, r, l, c, d, s, lec), (g, gm), sups)
    }.toSeq)

    db.run(action)
  }

  private def collectDependenciesMin[A, B](query: Query[ScheduleEntryTable, ScheduleEntryDb, Seq])
                                          (build: (ScheduleEntryDb, (GroupDb, Seq[GroupMembership]), Seq[(ScheduleEntrySupervisor, DbUser)]) => A)
                                          (transform: (Seq[A]) => Vector[B]): Future[Vector[B]] = {
    val mandatory = for {
      q <- query
      g <- q.groupFk
    } yield (q, g)

    val supervisors = for {
      s <- scheduleEntrySupervisorQuery
      u <- s.supervisorFk
    } yield (s, u)

    val group = groupQuery.joinLeft(groupMembershipQuery).on(_.id === _.group)

    val action = mandatory.joinLeft(supervisors).on(_._1.id === _._1.scheduleEntry).joinLeft(group).on(_._1._1.group === _._1.id).result.map(_.groupBy(_._1._1._1.id).map {
      case (id, dependencies) =>
        val (((se, g), _), _) = dependencies.find(_._1._1._1.id == id).get
        val sups = dependencies.flatMap(_._1._2)
        val gm = dependencies.flatMap(_._2.flatMap(_._2))

        build(se, (g, gm), sups)
    }.toSeq)

    db.run(action map transform)
  }

  /*
  val comps = all
        .filter(_.labwork.course.semesterIndex == labwork.course.semesterIndex)
        .filter(_.labwork.semester.id == labwork.semester.id)
        .filter(_.labwork.degree.id == labwork.degree.id)
        .filterNot(_.labwork.id == labwork.id)
   */

  def competitive(labwork: PostgresLabworkAtom): Future[Vector[ScheduleGen]] = {
    val comps = for {
      t <- tableQuery
      l <- t.labworkFk if l.id =!= labwork.id
      c <- l.courseFk if c.semesterIndex === labwork.course.semesterIndex
      s <- l.semesterFk if s.id === labwork.semester.id
      d <- l.degreeFk if d.id === labwork.degree.id
    } yield t

    collectDependenciesMin(comps) {
      case (se, (g, gm), sups) => (ScheduleEntryGen(
          se.start.localTime,
          se.end.localTime,
          se.date.localDate,
          se.room,
          sups.map(_._1.supervisor).toSet,
          g.copy(members = gm.map(_.student).toSet).toLwmModel
        ), se.labwork)
    } { entries =>
      entries.groupBy(_._2).map {
        case (id, e) => ScheduleGen(id, e.map(_._1).toVector)
      }.toVector
    }
  }
}

final class ScheduleEntryDaoImpl(val db: PostgresDriver.backend.Database) extends ScheduleEntryDao
