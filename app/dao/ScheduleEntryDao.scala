package dao

import java.util.UUID

import dao.helper.{CrossInvalidated, DatabaseExpander, TableFilter}
import database._
import javax.inject.Inject
import models.genesis.{ScheduleEntryGen, ScheduleGen}
import models.{genesis, _}
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps._

import scala.concurrent.{ExecutionContext, Future}

object ScheduleEntryDao extends TableFilter[ScheduleEntryTable] {
  def supervisorFilter(supervisor: UUID): TableFilterPredicate = e => TableQuery[ScheduleEntrySupervisorTable].filter(s => s.scheduleEntry === e.id && s.user === supervisor).exists
}

trait ScheduleEntryDao
  extends AbstractDao[ScheduleEntryTable, ScheduleEntryDb, ScheduleEntryLike]
    with CrossInvalidated[ScheduleEntryTable, ScheduleEntryDb] {

  import dao.helper.TableFilter.labworkFilter

  override val tableQuery = TableQuery[ScheduleEntryTable]

  val scheduleEntrySupervisorQuery: TableQuery[ScheduleEntrySupervisorTable] = TableQuery[ScheduleEntrySupervisorTable]
  protected val groupQuery: TableQuery[GroupTable] = TableQuery[GroupTable]
  protected val groupMembershipQuery: TableQuery[GroupMembershipTable] = TableQuery[GroupMembershipTable]

  override protected def toAtomic(query: Query[ScheduleEntryTable, ScheduleEntryDb, Seq]): Future[Seq[ScheduleEntryLike]] = collectDependencies(query) {
    case ((e, r, lab, c, d, s, lec), g, subs) =>
      val labwork = {
        val course = CourseAtom(c.label, c.description, c.abbreviation, lec.toUniqueEntity, c.semesterIndex, c.id)
        LabworkAtom(lab.label, lab.description, s.toUniqueEntity, course, d.toUniqueEntity, lab.subscribable, lab.published, lab.id)
      }

      ScheduleEntryAtom(labwork, e.start.localTime, e.end.localTime, e.date.localDate, r.toUniqueEntity, subs.map(_._2.toUniqueEntity).toSet, g.toUniqueEntity, e.id)
  }

  override protected def toUniqueEntity(query: Query[ScheduleEntryTable, ScheduleEntryDb, Seq]): Future[Seq[ScheduleEntryLike]] = collectDependencies(query) {
    case ((e, _, _, _, _, _, _), _, subs) => ScheduleEntry(e.labwork, e.start.localTime, e.end.localTime, e.date.localDate, e.room, subs.map(_._1.supervisor).toSet, e.group, e.id)
  }

  override protected def existsQuery(entity: ScheduleEntryDb): Query[ScheduleEntryTable, ScheduleEntryDb, Seq] = {
    filterBy(List(TableFilter.idFilter(entity.id)))
  }

  override protected def shouldUpdate(existing: ScheduleEntryDb, toUpdate: ScheduleEntryDb): Boolean = {
    existing.labwork == toUpdate.labwork && existing.group == toUpdate.group
  }

  private def collectDependencies(query: Query[ScheduleEntryTable, ScheduleEntryDb, Seq])
    (build: ((ScheduleEntryDb, RoomDb, LabworkDb, CourseDb, DegreeDb, SemesterDb, UserDb), GroupDb, Seq[(ScheduleEntrySupervisor, UserDb)]) => ScheduleEntryLike) = {
    val mandatory = for {
      q <- query
      r <- q.roomFk
      g <- q.groupFk
      l <- q.labworkFk
      c <- l.courseFk
      d <- l.degreeFk
      s <- l.semesterFk
      lec <- c.userFk
    } yield (q, r, g, l, c, d, s, lec)

    val supervisors = for {
      s <- scheduleEntrySupervisorQuery
      u <- s.userFk
    } yield (s, u)

    val group = groupQuery.joinLeft(groupMembershipQuery).on(_.id === _.group)

    val action = mandatory.joinLeft(supervisors).on(_._1.id === _._1.scheduleEntry).joinLeft(group).on(_._1._1.group === _._1.id).result.map(_.groupBy(_._1._1._1.id).map {
      case (id, dependencies) =>
        val (((se, r, g, l, c, d, s, lec), _), _) = dependencies.find(_._1._1._1.id == id).get
        val sups = dependencies.flatMap(_._1._2)
        val gm = dependencies.flatMap(_._2.flatMap(_._2))

        build((se, r, l, c, d, s, lec), g.copy(members = gm.map(_.student).toSet), sups)
    }.toSeq)

    db.run(action)
  }

  private def collectDependenciesMin[A, B](query: Query[ScheduleEntryTable, ScheduleEntryDb, Seq])
    (build: (ScheduleEntryDb, GroupDb, Seq[(ScheduleEntrySupervisor, UserDb)]) => A)
    (transform: Traversable[A] => Vector[B]): Future[Vector[B]] = {
    val mandatory = for {
      q <- query
      g <- q.groupFk
    } yield (q, g)

    val supervisors = for {
      s <- scheduleEntrySupervisorQuery
      u <- s.userFk
    } yield (s, u)

    val group = groupQuery.joinLeft(groupMembershipQuery).on(_.id === _.group)

    val action = mandatory.joinLeft(supervisors).on(_._1.id === _._1.scheduleEntry).joinLeft(group).on(_._1._1.group === _._1.id).result.map(_.groupBy(_._1._1._1.id).map {
      case (id, dependencies) =>
        val (((se, g), _), _) = dependencies.find(_._1._1._1.id == id).get
        val sups = dependencies.flatMap(_._1._2)
        val gm = dependencies.flatMap(_._2.flatMap(_._2))

        build(se, g.copy(members = gm.map(_.student).toSet), sups)
    })

    db.run(action map transform)
  }

  override protected val databaseExpander: Option[DatabaseExpander[ScheduleEntryDb]] = Some(new DatabaseExpander[ScheduleEntryDb] {
    override def expandCreationOf[E <: Effect](entities: ScheduleEntryDb*) = for {
      _ <- scheduleEntrySupervisorQuery ++= entities.flatMap { e =>
        e.supervisor.map(u => ScheduleEntrySupervisor(e.id, u))
      }
    } yield entities

    override def expandDeleteOf(entity: ScheduleEntryDb) = for {
      _ <- scheduleEntrySupervisorQuery.filter(_.scheduleEntry === entity.id).delete
    } yield entity

    override def expandUpdateOf(entity: ScheduleEntryDb) = for {
      d <- expandDeleteOf(entity)
      c <- expandCreationOf(d)
    } yield c.head
  })

  def competitive(labwork: LabworkAtom, considerSemesterIndex: Boolean = true): Future[Vector[ScheduleGen]] = {
    val comps = tableQuery
      .flatMap(t => t.labworkFk.withFilter(l => l.id =!= labwork.id)
        .flatMap(l => (if (considerSemesterIndex) l.courseFk.withFilter(c => c.semesterIndex === labwork.course.semesterIndex) else l.courseFk)
          .flatMap(_ => l.semesterFk.withFilter(s => s.id === labwork.semester.id)
            .flatMap(_ => l.degreeFk.withFilter(d => d.id === labwork.degree.id)
              .map(_ => t)))))

    scheduleGen(filterValidOnly(comps))
  }

  def scheduleGenBy(labwork: UUID) =
    scheduleGen(filterValidOnly(labworkFilter(labwork)).take(1)).map(_.headOption)

  private def scheduleGen(query: Query[ScheduleEntryTable, ScheduleEntryDb, Seq]): Future[Vector[ScheduleGen]] = {
    collectDependenciesMin(query) {
      case (se, g, sups) => (ScheduleEntryGen(
        se.start.localTime,
        se.end.localTime,
        se.date.localDate,
        se.room,
        sups.map(_._1.supervisor).toSet,
        g.toUniqueEntity
      ), se.labwork)
    } { entries =>
      entries.groupBy(_._2).map {
        case (id, e) => genesis.ScheduleGen(id, e.map(_._1).toVector)
      }.toVector
    }
  }

  override protected val schemas: List[PostgresProfile.DDL] = List(
    tableQuery.schema,
    scheduleEntrySupervisorQuery.schema
  )
}

final class ScheduleEntryDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends ScheduleEntryDao
