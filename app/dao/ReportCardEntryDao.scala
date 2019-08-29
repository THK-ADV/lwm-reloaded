package dao

import java.util.UUID

import dao.helper.{DatabaseExpander, TableFilter}
import database._
import javax.inject.Inject
import models._
import slick.jdbc
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery
import utils.date.DateTimeOps._

import scala.concurrent.{ExecutionContext, Future}

object ReportCardEntryDao extends TableFilter[ReportCardEntryTable] {
  def scheduleEntryFilter(scheduleEntry: UUID): TableFilterPredicate = r => TableQuery[ScheduleEntryTable].filter { s => // TODO test
    val schedule = s.id === scheduleEntry
    val ordinary = s.room === r.room && s.start === r.start && s.end === r.end && s.date === r.date
    val rescheduled = TableQuery[ReportCardRescheduledTable].filter(rs => rs.reportCardEntry === r.id && rs.room === s.room && rs.start === s.start && rs.end === s.end && rs.date === s.date).exists
    val retry = TableQuery[ReportCardRetryTable].filter(rt => rt.reportCardEntry === r.id && rt.room === s.room && rt.start === s.start && rt.end === s.end && rt.date === s.date).exists

    schedule && (ordinary || rescheduled || retry)
  }.exists

  def indexFilter(index: Int): TableFilterPredicate = _.assignmentIndex === index
}

trait ReportCardEntryDao extends AbstractDao[ReportCardEntryTable, ReportCardEntryDb, ReportCardEntryLike] {

  override val tableQuery = TableQuery[ReportCardEntryTable]

  val entryTypeQuery: TableQuery[ReportCardEntryTypeTable] = TableQuery[ReportCardEntryTypeTable]
  val retryQuery: TableQuery[ReportCardRetryTable] = TableQuery[ReportCardRetryTable]
  val rescheduledQuery: TableQuery[ReportCardRescheduledTable] = TableQuery[ReportCardRescheduledTable]

  def attendeeEmailAddressesOf(labwork: UUID) = {
    val query = for {
      q <- filterValidOnly(e => TableFilter.labworkFilter(labwork).apply(e))
      u <- q.userFk
    } yield u.email

    db run query.distinct.result
  }

  override protected def toAtomic(query: Query[ReportCardEntryTable, ReportCardEntryDb, Seq]): Future[Seq[ReportCardEntryLike]] = collectDependencies(query) {
    case ((entry, labwork, student, room), optRs, optRt, entryTypes) => ReportCardEntryAtom(
      student.toUniqueEntity,
      labwork.toUniqueEntity,
      entry.label,
      entry.date.localDate,
      entry.start.localTime,
      entry.end.localTime,
      room.toUniqueEntity,
      entryTypes.map(_.toUniqueEntity).toSet,
      optRs.map { case (rs, r) => ReportCardRescheduledAtom(rs.date.localDate, rs.start.localTime, rs.end.localTime, r.toUniqueEntity, rs.reason, rs.id) },
      optRt.map { case (rt, r) => ReportCardRetryAtom(rt.date.localDate, rt.start.localTime, rt.end.localTime, r.toUniqueEntity, rt.entryTypes.map(_.toUniqueEntity), rt.reason, rt.id) },
      entry.id
    )
  }

  override protected def toUniqueEntity(query: Query[ReportCardEntryTable, ReportCardEntryDb, Seq]): Future[Seq[ReportCardEntryLike]] = collectDependencies(query) {
    case ((entry, _, _, _), optRs, optRt, entryTypes) => entry.copy(entryTypes = entryTypes.toSet, rescheduled = optRs.map(_._1), retry = optRt.map(_._1)).toUniqueEntity
  }

  private def collectDependencies(query: Query[ReportCardEntryTable, ReportCardEntryDb, Seq])
    (build: ((ReportCardEntryDb, LabworkDb, UserDb, RoomDb), Option[(ReportCardRescheduledDb, RoomDb)], Option[(ReportCardRetryDb, RoomDb)], Seq[ReportCardEntryTypeDb]) => ReportCardEntryLike) = {
    val mandatory = for {
      q <- query
      l <- q.labworkFk
      s <- q.userFk
      r <- q.roomFk
    } yield (q, l, s, r)

    val retries = for {
      rt <- retryQuery.joinLeft(entryTypeQuery).on(_.id === _.reportCardRetry)
      r <- rt._1.roomFk
    } yield (rt, r)

    val rescheduled = for {
      rs <- rescheduledQuery
      r <- rs.roomFk
    } yield (rs, r)

    val action = mandatory.joinLeft(rescheduled).on(_._1.id === _._1.reportCardEntry).joinLeft(retries).on(_._1._1.id === _._1._1.reportCardEntry).joinLeft(entryTypeQuery).on(_._1._1._1.id === _.reportCardEntry).result.map(_.groupBy(_._1._1._1._1.id).map {
      case (id, dependencies) =>
        val (((entry, rescheduled), retry), _) = dependencies.find(_._1._1._1._1.id == id).get // lhs first, which should be the grouped key
        val retryEntryTypes = dependencies.flatMap(_._1._2.flatMap(_._1._2)) // resolve other n to m relationship
        val entryTypes = dependencies.flatMap(_._2) // rhs next, which should be the grouped values, the reason we grouped for
        val retryWithEntryTypes = retry.map(t => (t._1._1.copy(entryTypes = retryEntryTypes.toSet), t._2))

        build(entry, rescheduled, retryWithEntryTypes, entryTypes)
    }.toSeq)

    db.run(action)
  }

  override protected def existsQuery(entity: ReportCardEntryDb): Query[ReportCardEntryTable, ReportCardEntryDb, Seq] = {
    filterBy(List(
      TableFilter.labworkFilter(entity.labwork),
      TableFilter.userFilter(entity.student),
      ReportCardEntryDao.indexFilter(entity.assignmentIndex)
    ))
  }

  override protected def shouldUpdate(existing: ReportCardEntryDb, toUpdate: ReportCardEntryDb): Boolean = {
    import utils.date.DateTimeOps.{SqlDateConverter, SqlTimeConverter}

    (existing.date.localDate != toUpdate.date.localDate ||
      existing.start.localTime != toUpdate.start.localTime ||
      existing.end.localTime != toUpdate.end.localTime ||
      existing.room != toUpdate.room ||
      existing.entryTypes != toUpdate.entryTypes ||
      existing.rescheduled != toUpdate.rescheduled ||
      existing.retry != toUpdate.retry ||
      existing.label != toUpdate.label) &&
      (existing.assignmentIndex == toUpdate.assignmentIndex &&
        existing.labwork == toUpdate.labwork &&
        existing.student == toUpdate.student
        )
  }

  override protected val databaseExpander: Option[DatabaseExpander[ReportCardEntryDb]] = Some(new DatabaseExpander[ReportCardEntryDb] {
    override def expandCreationOf[E <: Effect](entities: ReportCardEntryDb*): jdbc.PostgresProfile.api.DBIOAction[Seq[ReportCardEntryDb], jdbc.PostgresProfile.api.NoStream, Effect.Write with Any] = { // entry -> types, rescheduled, (retry -> types)
      val rts = entities.flatMap(_.retry)
      val rtTypes = rts.flatMap(_.entryTypes)

      for {
        _ <- rescheduledQuery ++= entities.flatMap(_.rescheduled)
        _ <- retryQuery ++= rts
        _ <- entryTypeQuery ++= entities.flatMap(_.entryTypes) ++ rtTypes
      } yield entities
    }

    override def expandDeleteOf(entity: ReportCardEntryDb) = { // entry -> types, rescheduled, (retry -> types)
      val rs = rescheduledQuery.filter(_.reportCardEntry === entity.id)
      val rt = retryQuery.filter(_.reportCardEntry === entity.id)
      val types = entryTypeQuery.filter(t => t.reportCardEntry === entity.id || t.reportCardRetry.in(rt.map(_.id)))

      for {
        _ <- types.delete
        _ <- rt.delete
        _ <- rs.delete
      } yield entity
    }

    override def expandUpdateOf(entity: ReportCardEntryDb) = DBIO.successful(entity) // entry only
  })

  override protected val schemas: List[PostgresProfile.DDL] = List(
    tableQuery.schema,
    rescheduledQuery.schema,
    retryQuery.schema,
    entryTypeQuery.schema
  )
}

final class ReportCardEntryDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends ReportCardEntryDao