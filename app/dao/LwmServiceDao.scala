package dao

import java.util.UUID

import models.{LabworkApplicationDb, ReportCardEntryDb, ReportCardEntryTypeDb}
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._

trait LwmServiceDao {
  protected def db: PostgresDriver.backend.Database
  protected def labworkApplicationDao: LabworkApplicationDao
  protected def groupDao: GroupDao
  protected def reportCardEntryDao: ReportCardEntryDao

  import scala.concurrent.ExecutionContext.Implicits.global

  final def addStudentToLabwork(student: UUID, labwork: UUID, group: UUID) = {
    val action = for {
      lapp <- labworkApplicationDao.createQuery(LabworkApplicationDb(labwork, student, Set.empty))
      membership <- groupDao.add(student, group)
      srcStudent <- groupDao.randomStudentIn(group)
      templates <- reportCardEntryDao.reportCardsFrom(srcStudent, labwork)
      copied = templates.map { e =>
        val id = UUID.randomUUID
        val types = e.entryTypes.map(t => ReportCardEntryTypeDb(Some(id), None, t.entryType))

        ReportCardEntryDb(student, labwork, e.label, e.date, e.start, e.end, e.room, types, id = id)
      }
      _ <- reportCardEntryDao.createManyExpanded(copied)
    } yield (lapp, membership, copied)

    db.run(action.transactionally)
  }
}

final class LwmServiceDaoImpl(val db: PostgresDriver.backend.Database,
                              val labworkApplicationDao: LabworkApplicationDao,
                              val groupDao: GroupDao,
                              val reportCardEntryDao: ReportCardEntryDao) extends LwmServiceDao
