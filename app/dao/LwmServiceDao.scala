package dao

import java.util.UUID

import dao.helper.{Core, TableFilter}
import database.{GroupMembership, LabworkApplicationDb, ReportCardEntryDb, ReportCardEntryTypeDb}
import javax.inject.Inject
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

trait LwmServiceDao extends Core {
  import TableFilter.{userFilter, labworkFilter}

  protected def labworkApplicationDao: LabworkApplicationDao

  protected def groupDao: GroupDao

  protected def reportCardEntryDao: ReportCardEntryDao

  def insertStudentToGroup(student: UUID, labwork: UUID, destGroup: UUID): Future[(LabworkApplicationDb, GroupMembership, Option[UUID], Seq[ReportCardEntryDb])] = {
    val result = for {
      membership <- groupDao.add(student, destGroup)
      maybeApp <- labworkApplicationDao.filterBy(List(labworkFilter(labwork), userFilter(student))).result.headOption
      app <- maybeApp.fold(labworkApplicationDao.createQuery(LabworkApplicationDb(labwork, student, Set.empty)))(DBIO.successful)
      srcStudent <- groupDao.firstStudentIn(destGroup)
      srcCards <- DBIO.sequenceOption(srcStudent.map(s => reportCardEntryDao.filterBy(List(labworkFilter(labwork), userFilter(s))).result))
      copied = srcCards.getOrElse(Seq.empty).map { card =>
        val newId = UUID.randomUUID
        val newEntryTypes = card.entryTypes.map(t => ReportCardEntryTypeDb(Some(newId), None, t.entryType))

        ReportCardEntryDb(student, labwork, card.label, card.date, card.start, card.end, card.room, newEntryTypes, id = newId)
      }
      destCards <- reportCardEntryDao.createManyQuery(copied)
    } yield (app, membership, srcStudent, destCards)

    db.run(result.transactionally)
  }
}

final class LwmServiceDaoImpl @Inject()(
  val db: Database,
  val executionContext: ExecutionContext,
  val labworkApplicationDao: LabworkApplicationDao,
  val groupDao: GroupDao,
  val reportCardEntryDao: ReportCardEntryDao
) extends LwmServiceDao
