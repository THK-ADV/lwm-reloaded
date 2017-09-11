package dao

import java.util.UUID

import models.{LabworkApplicationDb, ReportCardEntryDb, ReportCardEntryTypeDb}
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import store.UserTable

trait LwmServiceDao {
  protected def db: PostgresDriver.backend.Database
  protected def labworkApplicationDao: LabworkApplicationDao
  protected def groupDao: GroupDao
  protected def reportCardEntryDao: ReportCardEntryDao

  import scala.concurrent.ExecutionContext.Implicits.global

  final def addStudentToLabwork(student: UUID, labwork: UUID, destGroup: UUID) = {
    val action = for {
      lapp <- labworkApplicationDao.createQuery(LabworkApplicationDb(labwork, student, Set.empty))
      srcStudent <- groupDao.randomStudentIn(destGroup)
      membership <- groupDao.add(student, destGroup)
      templates <- reportCardEntryDao.reportCardsFrom(srcStudent, labwork)
      copied = templates.map { e =>
        val newId = UUID.randomUUID
        val newTypes = e.entryTypes.map(t => ReportCardEntryTypeDb(Some(newId), None, t.entryType))

        ReportCardEntryDb(student, labwork, e.label, e.date, e.start, e.end, e.room, newTypes, id = newId)
      }
      _ <- reportCardEntryDao.createManyExpanded(copied)
    } yield (lapp, membership, copied)

    db.run(action.transactionally)
  }

  final def swapStudentsInGroup(student: UUID, labwork: UUID, destGroup: UUID) = {
    val action = for {
      destStudent <- groupDao.randomStudentIn(destGroup)
      srcGroup <- groupDao.groupOf(student, labwork)
      membership <- groupDao.swap(student, srcGroup, destGroup)

      templateEntries <- reportCardEntryDao.reportCardsFrom(destStudent, labwork)
      studentEntries <- reportCardEntryDao.reportCardsFrom(student, labwork)
      updatedStudentEntries = for {
        (origin, template) <- sorted(studentEntries).zip(sorted(templateEntries)) if origin.label == template.label
      } yield origin.copy(date = template.date, start = template.start, end = template.end, room = template.room)
      updated <- reportCardEntryDao.updateManyQuery(updatedStudentEntries.toList)
    } yield (membership, studentEntries, updated)

    db.run(action.transactionally)
  }

  final def multipleLabworkApplications(course: String) = {
    val action = labworkApplicationDao.tableQuery
      .filter(_.isCurrentSemester)
      .filter(_.hasCourse(course))
      .join(TableQuery[UserTable]).on(_.applicant === _.id)
      .result.map(_.groupBy(_._2.systemId).filter {
        case (_, apps) => apps.groupBy(_._1.labwork).exists(_._2.size > 1)
      }
      .mapValues(_.map(_._1)))

    db.run(action)
  }

  protected def sorted(entries: Seq[ReportCardEntryDb]): Seq[ReportCardEntryDb] = {
    import models.LwmDateTime._

    entries.sortBy(e => e.date.localDate.toLocalDateTime(e.start.localTime))
  }
}

final class LwmServiceDaoImpl(val db: PostgresDriver.backend.Database,
                              val labworkApplicationDao: LabworkApplicationDao,
                              val groupDao: GroupDao,
                              val reportCardEntryDao: ReportCardEntryDao) extends LwmServiceDao
