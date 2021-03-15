package dao

import dao.helper.{CrossInvalidated, DatabaseExpander, TableFilter}
import database.{LabworkDb, ReportCardEntryDb, RoomDb, UserDb, _}
import models.AnnotationLike.AnnotationAtom
import models._
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.lifted.{Rep, TableQuery}
import utils.date.DateTimeOps._

import java.sql.{Date, Time}
import java.util.UUID
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

object ReportCardEntryDao extends TableFilter[ReportCardEntryTable] {

  def scheduleEntryFilter(scheduleEntry: UUID): TableFilterPredicate = r => TableQuery[ScheduleEntryTable].filter { s =>
    val schedule = s.id === scheduleEntry
    val inGrp = TableQuery[GroupMembershipTable].filter(g => g.group === s.group && g.user === r.user).exists
    val ordinary = inGrp && s.room === r.room && s.start === r.start && s.end === r.end && s.date === r.date
    val rescheduled = isRescheduled(r, s.date, s.start, s.end, s.room) // TODO use latest once

    schedule && (ordinary || rescheduled)
  }.exists

  def precisedAppointmentFilter(
    course: UUID,
    semester: UUID,
    date: Rep[Date],
    start: Rep[Time],
    end: Rep[Time],
    room: RoomTable
  ): TableFilterPredicate = r => {
    val entry = r.memberOfCourse(course) && r.inSemester(semester)
    val ordinary = room.id === r.room && start === r.start && end === r.end && date === r.date
    val rescheduled = isRescheduled(r, date, start, end, room.id)

    entry && (ordinary || rescheduled)
  }

  private def isRescheduled(
    r: ReportCardEntryTable,
    date: Rep[Date],
    start: Rep[Time],
    end: Rep[Time],
    room: Rep[UUID]
  ) = TableQuery[ReportCardRescheduledTable].filter(rs =>
    rs.reportCardEntry === r.id &&
      rs.room === room &&
      rs.start === start &&
      rs.end === end &&
      rs.date === date
  ).sorted(_.lastModified).take(1).exists

  def indexFilter(index: Int): TableFilterPredicate = _.assignmentIndex === index
}

trait ReportCardEntryDao
  extends AbstractDao[ReportCardEntryTable, ReportCardEntryDb, ReportCardEntryLike]
    with CrossInvalidated[ReportCardEntryTable, ReportCardEntryDb] {

  override val tableQuery = TableQuery[ReportCardEntryTable]

  val entryTypeQuery: TableQuery[ReportCardEntryTypeTable] = TableQuery[ReportCardEntryTypeTable]
  val rescheduledQuery: TableQuery[ReportCardRescheduledTable] = TableQuery[ReportCardRescheduledTable]

  def attendeeEmailAddressesOf(labwork: UUID) = {
    val query = for {
      q <- filterValidOnly(e => TableFilter.labworkFilter(labwork).apply(e))
      u <- q.userFk
    } yield u.email

    db run query.distinct.result
  }

  def numberOfStudents(course: UUID, labwork: UUID) = {
    val query = filterValidOnly(t => t.memberOfCourse(course) && t.labwork === labwork)
      .groupBy(_.user)
      .map(_._1)
      .length

    db run query.result
  }

  override protected def toAtomic(query: Query[ReportCardEntryTable, ReportCardEntryDb, Seq]): Future[Seq[ReportCardEntryLike]] =
    collectDependencies(query) {
      case ((e, l, s, u)) => makeAtomicModel(e, l, s, u)
    }

  override def toUniqueEntity(query: Query[ReportCardEntryTable, ReportCardEntryDb, Seq]): Future[Seq[ReportCardEntryLike]] =
    collectDependencies(query) {
      case ((e, _, _, _)) => makeNonAtomicModel(e)
    }

  private def baseQuery(query: Query[ReportCardEntryTable, ReportCardEntryDb, Seq]) = for {
    (q, ts) <- query.joinLeft(entryTypeQuery).on(_.id === _.reportCardEntry) if q.isValid
    l <- q.labworkFk
    s <- q.userFk
    r <- q.roomFk
  } yield (q, l, s, r, ts)

  private def reschedulesQuery = for {
    rs <- rescheduledQuery if rs.isValid
    r <- rs.roomFk
  } yield (rs, r)

  private def annotationQuery = for {
    q <- TableQuery[AnnotationTable] if q.isValid
    u <- q.userFk
  } yield (q, u)

  def withAnnotations(query: Query[ReportCardEntryTable, ReportCardEntryDb, Seq], atomic: Boolean): Future[Seq[(ReportCardEntryLike, Set[AnnotationLike])]] = {
    val action = baseQuery(query)
      .joinLeft(annotationQuery).on(_._1.id === _._1.reportCardEntry)
      .result.map(_.groupBy(_._1._1.id).map {
      case (_, dependencies) =>
        val (e, l, s, r) = makeEntry(dependencies.map(_._1))
        val as = dependencies.flatMap(_._2)

        if (atomic)
          (makeAtomicModel(e, l, s, r), as.map(a => makeAnnotationAtom(a._1, a._2)).toSet)
        else
          (makeNonAtomicModel(e), as.map(a => makeAnnotation(a._1)).toSet)
    }.toSeq)

    db.run(action)
  }

  def withReschedules(query: Query[ReportCardEntryTable, ReportCardEntryDb, Seq], atomic: Boolean): Future[Seq[(ReportCardEntryLike, Set[ReportCardRescheduledLike])]] = {
    val action = baseQuery(query)
      .joinLeft(reschedulesQuery)
      .on(_._1.id === _._1.reportCardEntry)
      .result
      .map(_.groupBy(_._1._1.id).map {
        case (_, dependencies) =>
          val (e, l, s, r) = makeEntry(dependencies.map(_._1))
          val rs = dependencies.flatMap(_._2)

          if (atomic)
            (makeAtomicModel(e, l, s, r), rs.map(a => makeRescheduleAtom(a._1, a._2)).toSet)
          else
            (makeNonAtomicModel(e), rs.map(a => makeReschedule(a._1)).toSet)
      }.toSeq)

    db.run(action)
  }

  def withAnnotationsAndReschedules(query: Query[ReportCardEntryTable, ReportCardEntryDb, Seq], atomic: Boolean): Future[Seq[(ReportCardEntryLike, Set[AnnotationLike], Set[ReportCardRescheduledLike])]] = {
    val action = baseQuery(query)
      .joinLeft(annotationQuery).on(_._1.id === _._1.reportCardEntry)
      .joinLeft(reschedulesQuery).on(_._1._1.id === _._1.reportCardEntry)
      .result
      .map(_.groupBy(_._1._1._1.id).map {
        case (_, dependencies) =>
          val (e, l, s, r) = makeEntry(dependencies.map(_._1._1))
          val as = dependencies.flatMap(_._1._2)
          val rs = dependencies.flatMap(_._2)

          if (atomic)
            (
              makeAtomicModel(e, l, s, r),
              as.map(a => makeAnnotationAtom(a._1, a._2)).toSet,
              rs.map(a => makeRescheduleAtom(a._1, a._2)).toSet
            )
          else
            (
              makeNonAtomicModel(e),
              as.map(a => makeAnnotation(a._1)).toSet,
              rs.map(a => makeReschedule(a._1)).toSet
            )
      }.toSeq)

    db.run(action)
  }

  private def makeReschedule(rs: ReportCardRescheduledDb): ReportCardRescheduledLike =
    rs.toUniqueEntity

  private def makeRescheduleAtom(rs: ReportCardRescheduledDb, r: RoomDb): ReportCardRescheduledLike =
    ReportCardRescheduledAtom(
      rs.date.localDate,
      rs.start.localTime,
      rs.end.localTime,
      r.toUniqueEntity,
      rs.reason,
      rs.lastModified.dateTime,
      rs.id
    )

  private def makeEntry(rts: Seq[(ReportCardEntryDb, LabworkDb, UserDb, RoomDb, Option[ReportCardEntryTypeDb])]) = {
    val (e, l, u, r, _) = rts.head
    (e.copy(entryTypes = rts.flatMap(_._5).toSet), l, u, r)
  }

  private def makeNonAtomicModel(entry: ReportCardEntryDb) =
    entry.toUniqueEntity

  private def makeAtomicModel(
    entry: ReportCardEntryDb,
    labwork: LabworkDb,
    student: UserDb,
    room: RoomDb
  ): ReportCardEntryLike =
    ReportCardEntryAtom(
      student.toUniqueEntity,
      labwork.toUniqueEntity,
      entry.label,
      entry.date.localDate,
      entry.start.localTime,
      entry.end.localTime,
      room.toUniqueEntity,
      entry.entryTypes.map(_.toUniqueEntity),
      entry.assignmentIndex,
      entry.id
    )

  private def makeAnnotationAtom(a: AnnotationDb, u: UserDb): AnnotationLike =
    AnnotationAtom(
      a.reportCardEntry,
      u.toUniqueEntity,
      a.message,
      a.lastModified.dateTime,
      a.id
    )

  private def makeAnnotation(a: AnnotationDb): AnnotationLike =
    a.toUniqueEntity

  private def collectDependencies(query: Query[ReportCardEntryTable, ReportCardEntryDb, Seq])
    (build: ((ReportCardEntryDb, LabworkDb, UserDb, RoomDb)) => ReportCardEntryLike) = {
    val action = baseQuery(query).result.map(_.groupBy(_._1.id).map {
      case (_, dependencies) =>
        build(makeEntry(dependencies))
    }.toSeq)

    db.run(action)
  }

  // TODO remove?
  def withEntryTypes(preds: List[ReportCardEntryTable => Rep[Boolean]]): DBIOAction[Seq[ReportCardEntryDb], NoStream, Effect.Read] =
    filterBy(preds)
      .joinLeft(entryTypeQuery)
      .on(_.id === _.reportCardEntry)
      .result
      .map(_.groupBy(_._1.id).map {
        case (id, dep) =>
          val (entry, _) = dep.find(_._1.id == id).get
          val entryTypes = dep.flatMap(_._2)

          entry.copy(entryTypes = entryTypes.toSet)
      }.toSeq)

  override protected def existsQuery(entity: ReportCardEntryDb): Query[ReportCardEntryTable, ReportCardEntryDb, Seq] = {
    filterBy(List(
      TableFilter.labworkFilter(entity.labwork),
      TableFilter.userFilter(entity.student),
      ReportCardEntryDao.indexFilter(entity.assignmentIndex)
    ))
  }

  override protected def shouldUpdate(existing: ReportCardEntryDb, toUpdate: ReportCardEntryDb): Boolean = {
    existing.assignmentIndex == toUpdate.assignmentIndex &&
      existing.labwork == toUpdate.labwork &&
      existing.student == toUpdate.student
  }

  override protected val databaseExpander: Option[DatabaseExpander[ReportCardEntryDb]] = Some(new DatabaseExpander[ReportCardEntryDb] {

    override def expandCreationOf[E <: Effect](entities: ReportCardEntryDb*): DBIOAction[Seq[ReportCardEntryDb], NoStream, Effect.Write with Any] = {
      for {
        _ <- entryTypeQuery ++= entities.flatMap(_.entryTypes)
      } yield entities
    }

    override def expandDeleteOf(entity: ReportCardEntryDb) = {
      for {
        _ <- entryTypeQuery.filter(t => t.reportCardEntry === entity.id).delete
      } yield entity
    }

    override def expandUpdateOf(entity: ReportCardEntryDb) =
      DBIO.successful(entity) // entry only
  })

  override protected val schemas: List[PostgresProfile.DDL] = List(
    tableQuery.schema,
    entryTypeQuery.schema
  )
}

final class ReportCardEntryDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends ReportCardEntryDao