package dao

import java.util.UUID

import utils.LwmDateTime._
import models._
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery
import store._

import scala.concurrent.Future

//case class ReportCardEntryStudentFilter(value: String) extends TableFilter[ReportCardEntryTable] {
//  override def predicate = _.student === UUID.fromString(value)
//}
//
//case class ReportCardEntryLabworkFilter(value: String) extends TableFilter[ReportCardEntryTable] {
//  override def predicate = _.labwork === UUID.fromString(value)
//}
//
//case class ReportCardEntryCourseFilter(value: String) extends TableFilter[ReportCardEntryTable] {
//  override def predicate = _.memberOfCourse(value)
//}
//
//case class ReportCardEntryRoomFilter(value: String) extends TableFilter[ReportCardEntryTable] {
//  override def predicate = _.room === UUID.fromString(value)
//}
//
//case class ReportCardEntryDateFilter(value: String) extends TableFilter[ReportCardEntryTable] {
//  override def predicate = _.onDate(value)
//}
//
//case class ReportCardEntryStartFilter(value: String) extends TableFilter[ReportCardEntryTable] {
//  override def predicate = _.onStart(value)
//}
//
//case class ReportCardEntryEndFilter(value: String) extends TableFilter[ReportCardEntryTable] {
//  override def predicate = _.onEnd(value)
//}
//
//case class ReportCardEntrySinceFilter(value: String) extends TableFilter[ReportCardEntryTable] {
//  override def predicate = _.since(value)
//}
//
//case class ReportCardEntryUntilFilter(value: String) extends TableFilter[ReportCardEntryTable] {
//  override def predicate = _.until(value)
//}
//
//case class ReportCardEntryScheduleEntryFilter(value: String) extends TableFilter[ReportCardEntryTable] {
//  override def predicate = r => TableQuery[ScheduleEntryTable].filter { s =>
//    val schedule = s.id === UUID.fromString(value)
//    val ordinary = s.room === r.room && s.start === r.start && s.end === r.end && s.date === r.date
//    val rescheduled = TableQuery[ReportCardRescheduledTable].filter(rs => rs.reportCardEntry === r.id && rs.room === s.room && rs.start === s.start && rs.end === s.end && rs.date === s.date).exists
//    val retry = TableQuery[ReportCardRetryTable].filter(rt => rt.reportCardEntry === r.id && rt.room === s.room && rt.start === s.start && rt.end === s.end && rt.date === s.date).exists
//
//    schedule && (ordinary || rescheduled || retry)
//  }.exists
//}
//
//trait ReportCardEntryDao extends AbstractDao[ReportCardEntryTable, ReportCardEntryDb, ReportCardEntry] {
//  import scala.concurrent.ExecutionContext.Implicits.global
//
//  override val tableQuery = TableQuery[ReportCardEntryTable]
//
//  protected val entryTypeQuery: TableQuery[ReportCardEntryTypeTable] = TableQuery[ReportCardEntryTypeTable]
//  protected val retryQuery: TableQuery[ReportCardRetryTable] = TableQuery[ReportCardRetryTable]
//  protected val rescheduledQuery: TableQuery[ReportCardRescheduledTable] = TableQuery[ReportCardRescheduledTable]
//
//  override protected def toAtomic(query: Query[ReportCardEntryTable, ReportCardEntryDb, Seq]): Future[Seq[ReportCardEntry]] = collectDependencies(query) {
//    case ((entry, labwork, student, room), optRs, optRt, entryTypes) => PostgresReportCardEntryAtom(
//      student.toLwmModel,
//      labwork.toLwmModel,
//      entry.label,
//      entry.date.localDate,
//      entry.start.localTime,
//      entry.end.localTime,
//      room.toLwmModel,
//      entryTypes.map(_.toLwmModel).toSet,
//      optRs.map { case (rs, r) => PostgresReportCardRescheduledAtom(rs.date.localDate, rs.start.localTime, rs.end.localTime, r.toLwmModel, rs.reason, rs.id) },
//      optRt.map { case (rt, r) => PostgresReportCardRetryAtom(rt.date.localDate, rt.start.localTime, rt.end.localTime, r.toLwmModel, rt.entryTypes.map(_.toLwmModel), rt.reason, rt.id) },
//      entry.id
//    )
//  }
//
//  override protected def toUniqueEntity(query: Query[ReportCardEntryTable, ReportCardEntryDb, Seq]): Future[Seq[ReportCardEntry]] = collectDependencies(query) {
//    case ((entry, _, _, _), optRs, optRt, entryTypes) => entry.copy(entryTypes = entryTypes.toSet, retry = optRt.map(_._1), rescheduled = optRs.map(_._1)).toLwmModel
//  }
//
//  private def collectDependencies(query: Query[ReportCardEntryTable, ReportCardEntryDb, Seq])
//                                 (build: ((ReportCardEntryDb, LabworkDb, DbUser, RoomDb), Option[(ReportCardRescheduledDb, RoomDb)], Option[(ReportCardRetryDb, RoomDb)], Seq[ReportCardEntryTypeDb]) => ReportCardEntry) = {
//    val mandatory = for {
//      q <- query
//      l <- q.labworkFk
//      s <- q.studentFk
//      r <- q.roomFk
//    } yield (q, l, s, r)
//
//    val retries = for {
//      rt <- retryQuery.joinLeft(entryTypeQuery).on(_.id === _.reportCardRetry)
//      r <- rt._1.roomFk
//    } yield (rt, r)
//
//    val rescheduled = for {
//      rs <- rescheduledQuery
//      r <- rs.roomFk
//    } yield (rs, r)
//
//    val action = mandatory.joinLeft(rescheduled).on(_._1.id === _._1.reportCardEntry).joinLeft(retries).on(_._1._1.id === _._1._1.reportCardEntry).joinLeft(entryTypeQuery).on(_._1._1._1.id === _.reportCardEntry).result.map(_.groupBy(_._1._1._1._1.id).map {
//      case (id, dependencies) =>
//        val (((entry, rescheduled), retry), _) = dependencies.find(_._1._1._1._1.id == id).get // lhs first, which should be the grouped key
//        val retryEntryTypes = dependencies.flatMap(_._1._2.flatMap(_._1._2)) // resolve other n to m relationship
//        val entryTypes = dependencies.flatMap(_._2) // rhs next, which should be the grouped values, the reason we grouped for
//        val retryWithEntryTypes = retry.map(t => (t._1._1.copy(entryTypes = retryEntryTypes.toSet), t._2))
//
//        build(entry, rescheduled, retryWithEntryTypes, entryTypes)
//    }.toSeq)
//
//    db.run(action)
//  }
//
//  override protected def existsQuery(entity: ReportCardEntryDb): Query[ReportCardEntryTable, ReportCardEntryDb, Seq] = {
//    filterBy(List(
//      ReportCardEntryLabworkFilter(entity.labwork.toString),
//      ReportCardEntryStudentFilter(entity.student.toString)
//    ))
//  }
//
//  override protected def shouldUpdate(existing: ReportCardEntryDb, toUpdate: ReportCardEntryDb): Boolean = {
//    existing.labwork == toUpdate.labwork && existing.student == toUpdate.student
//  }
//
//  override protected def databaseExpander: Option[DatabaseExpander[ReportCardEntryDb]] = Some(new DatabaseExpander[ReportCardEntryDb] {
//    override def expandCreationOf[E <: Effect](entities: Seq[ReportCardEntryDb]) = { // entry -> types, rescheduled, (retry -> types)
//      val rts = entities.flatMap(_.retry)
//      val rtTypes = rts.flatMap(_.entryTypes)
//
//      for {
//        _ <- rescheduledQuery ++= entities.flatMap(_.rescheduled)
//        _ <- retryQuery ++= rts
//        _ <- entryTypeQuery ++= entities.flatMap(_.entryTypes) ++ rtTypes
//      } yield entities
//    }
//
//    override def expandDeleteOf(entity: ReportCardEntryDb) = { // entry -> types, rescheduled, (retry -> types)
//      val rs = rescheduledQuery.filter(_.reportCardEntry === entity.id)
//      val rt = retryQuery.filter(_.reportCardEntry === entity.id)
//      val types = entryTypeQuery.filter(t => t.reportCardEntry === entity.id || t.reportCardRetry.in(rt.map(_.id)))
//
//      val deleted = for {
//        d1 <- types.delete
//        d2 <- rt.delete
//        d3 <- rs.delete
//      } yield d1 + d2 + d3
//
//      deleted.map(_ => Some(entity))
//    }
//
//    override def expandUpdateOf(entity: ReportCardEntryDb) = DBIO.successful(Some(entity)) // entry only
//  })
//
//  private lazy val schemas = List(
//    tableQuery.schema,
//    rescheduledQuery.schema,
//    retryQuery.schema,
//    entryTypeQuery.schema
//  )
//
//  override def createSchema: Future[Unit] = {
//    db.run(DBIO.seq(schemas.map(_.create): _*).transactionally)
//  }
//
//  override def dropSchema: Future[Unit] = {
//    db.run(DBIO.seq(schemas.reverseMap(_.drop): _*).transactionally)
//  }
//}
//
//final class ReportCardEntryDaoImpl(val database: PostgresDatabase) extends ReportCardEntryDao