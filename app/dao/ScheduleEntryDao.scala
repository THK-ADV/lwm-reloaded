package dao

import java.util.UUID

import dao.helper.DatabaseExpander
import database._
import javax.inject.Inject
import models.genesis.{ScheduleEntryGen, ScheduleGen}
import models.{genesis, _}
import slick.jdbc
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime._

import scala.concurrent.{ExecutionContext, Future}

case class ScheduleEntryLabworkFilter(value: String) extends TableFilter[ScheduleEntryTable] {
  override def predicate = _.labwork === UUID.fromString(value)
}

case class ScheduleEntryCourseFilter(value: String) extends TableFilter[ScheduleEntryTable] {
  override def predicate = _.memberOfCourse(value)
}

case class ScheduleEntryGroupFilter(value: String) extends TableFilter[ScheduleEntryTable] {
  override def predicate = _.group === UUID.fromString(value)
}

case class ScheduleEntrySupervisorFilter(value: String) extends TableFilter[ScheduleEntryTable] {
  override def predicate = e => TableQuery[ScheduleEntrySupervisorTable].filter(s => s.scheduleEntry === e.id && s.supervisor === UUID.fromString(value)).exists
}

case class ScheduleEntryDateFilter(value: String) extends TableFilter[ScheduleEntryTable] {
  override def predicate = _.onDate(value)
}

case class ScheduleEntryStartFilter(value: String) extends TableFilter[ScheduleEntryTable] {
  override def predicate = _.onStart(value)
}

case class ScheduleEntryEndFilter(value: String) extends TableFilter[ScheduleEntryTable] {
  override def predicate = _.onEnd(value)
}

case class ScheduleEntrySinceFilter(value: String) extends TableFilter[ScheduleEntryTable] {
  override def predicate = _.since(value)
}

case class ScheduleEntryUntilFilter(value: String) extends TableFilter[ScheduleEntryTable] {
  override def predicate = _.until(value)
}

trait ScheduleEntryDao extends AbstractDao[ScheduleEntryTable, ScheduleEntryDb, ScheduleEntryLike] {

  override val tableQuery = TableQuery[ScheduleEntryTable]

  val scheduleEntrySupervisorQuery: TableQuery[ScheduleEntrySupervisorTable] = TableQuery[ScheduleEntrySupervisorTable]
  protected val groupQuery: TableQuery[GroupTable] = TableQuery[GroupTable]
  protected val groupMembershipQuery: TableQuery[GroupMembershipTable] = TableQuery[GroupMembershipTable]

  override protected def toAtomic(query: Query[ScheduleEntryTable, ScheduleEntryDb, Seq]): Future[Traversable[ScheduleEntryLike]] = collectDependencies(query) {
    case ((e, r, lab, c, d, s, lec), g, subs) =>
      val labwork = {
        val course = CourseAtom(c.label, c.description, c.abbreviation, lec.toUniqueEntity, c.semesterIndex, c.id)
        LabworkAtom(lab.label, lab.description, s.toUniqueEntity, course, d.toUniqueEntity, lab.subscribable, lab.published, lab.id)
      }

      ScheduleEntryAtom(labwork, e.start.localTime, e.end.localTime, e.date.localDate, r.toUniqueEntity, subs.map(_._2.toUniqueEntity).toSet, g.toUniqueEntity, e.id)
  }

  override protected def toUniqueEntity(query: Query[ScheduleEntryTable, ScheduleEntryDb, Seq]): Future[Traversable[ScheduleEntryLike]] = collectDependencies(query) {
    case ((e, _, _, _, _, _, _), _, subs) => ScheduleEntry(e.labwork, e.start.localTime, e.end.localTime, e.date.localDate, e.room, subs.map(_._1.supervisor).toSet, e.group, e.id)
  }

  override protected def existsQuery(entity: ScheduleEntryDb): Query[ScheduleEntryTable, ScheduleEntryDb, Seq] = {
    filterBy(List(
      ScheduleEntryLabworkFilter(entity.labwork.toString),
      ScheduleEntryGroupFilter(entity.group.toString)
    ))
  }

  override protected def shouldUpdate(existing: ScheduleEntryDb, toUpdate: ScheduleEntryDb): Boolean = {
    import utils.LwmDateTime.{SqlDateConverter, TimeConverter}

    (existing.supervisor != toUpdate.supervisor ||
      existing.date.localDate != toUpdate.date.localDate ||
      existing.start.localTime != toUpdate.start.localTime ||
      existing.end.localTime != toUpdate.end.localTime ||
      existing.room != toUpdate.room) &&
      (existing.labwork == toUpdate.labwork && existing.group == toUpdate.group)
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

        build((se, r, l, c, d, s, lec), g.copy(members = gm.map(_.student).toSet), sups)
    })

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
      u <- s.supervisorFk
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

  override protected def databaseExpander: Option[DatabaseExpander[ScheduleEntryDb]] = Some(new DatabaseExpander[ScheduleEntryDb] {
    override def expandCreationOf[E <: Effect](entities: ScheduleEntryDb*): jdbc.PostgresProfile.api.DBIOAction[Seq[ScheduleEntryDb], jdbc.PostgresProfile.api.NoStream, Effect.Write with Any] = for {
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

  def scheduleGenBy(labworkId: String) = scheduleGen(filterValidOnly(_.labwork === UUID.fromString(labworkId)).take(1)).map(_.headOption)

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


  private lazy val schemas = List(
    tableQuery.schema,
    scheduleEntrySupervisorQuery.schema
  )

  override def createSchema: Future[Unit] = {
    db.run(DBIO.seq(schemas.map(_.create): _*).transactionally)
  }

  override def dropSchema: Future[Unit] = {
    db.run(DBIO.seq(schemas.reverseMap(_.drop): _*).transactionally)
  }
}

final class ScheduleEntryDaoImpl @Inject()(val db: PostgresProfile.backend.Database, val executionContext: ExecutionContext) extends ScheduleEntryDao
