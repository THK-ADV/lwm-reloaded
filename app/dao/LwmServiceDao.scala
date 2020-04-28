package dao

import java.util.UUID

import dao.helper.{Core, TableFilter}
import database._
import javax.inject.Inject
import models.{LabworkApplication, ReportCardEntry}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

trait LwmServiceDao extends Core {

  import TableFilter.{labworkFilter, userFilter}

  protected def labworkApplicationDao: LabworkApplicationDao

  protected def groupDao: GroupDao

  protected def reportCardEntryDao: ReportCardEntryDao

  def insertStudentToGroup(student: UUID, labwork: UUID, group: UUID): Future[(GroupMembership, LabworkApplication, Seq[ReportCardEntry], Option[UUID])] = {
    val result = for {
      membership <- groupDao.add(student, group)
      maybeApp <- labworkApplicationDao.filterBy(List(labworkFilter(labwork), userFilter(student))).result.headOption
      app <- maybeApp.fold(labworkApplicationDao.createQuery(LabworkApplicationDb(labwork, student, Set.empty)))(DBIO.successful)
      srcStudent <- groupDao.firstStudentIn(group)
      srcCards <- DBIO.sequenceOption(srcStudent.map(s => reportCards(labwork, s)))
      copied = srcCards.getOrElse(Seq.empty).map { card =>
        val newId = UUID.randomUUID
        val newEntryTypes = card.entryTypes.map(t => ReportCardEntryTypeDb(Some(newId), None, t.entryType))

        ReportCardEntryDb(student, labwork, card.label, card.date, card.start, card.end, card.room, newEntryTypes, card.assignmentIndex, id = newId)
      }
      destCards <- reportCardEntryDao.createManyQuery(copied)
    } yield (membership, app.toUniqueEntity, destCards.map(_.toUniqueEntity), srcStudent)

    db.run(result.transactionally)
  }

  // TODO remove entry types
  def removeStudentFromGroup(student: UUID, labwork: UUID, group: UUID): Future[(Boolean, LabworkApplication, Seq[ReportCardEntry])] = {
    val result = for {
      shouldContinue <- groupDao.groupHasAtLeastTwoMembers(group)
      _ <- ensure(shouldContinue, () => "there must be at least one member left after removal")

      removedMembership <- groupDao.remove(student, group)
      application = labworkApplicationDao.filterBy(List(labworkFilter(labwork), userFilter(student)))
      removedApplication <- labworkApplicationDao.invalidateSingleQuery(application)
      cards <- reportCards(labwork, student)
      removedCards <- DBIO.sequence(cards.map(c => reportCardEntryDao.invalidateSingle(c.id)))
    } yield (removedMembership > 0, removedApplication.toUniqueEntity, removedCards.map(_.toUniqueEntity))

    db.run(result.transactionally)
  }

  def moveStudentToGroup(student: UUID, labwork: UUID, srcGroup: UUID, destGroup: UUID): Future[(Boolean, GroupMembership, Seq[ReportCardEntry], Seq[ReportCardEntry], Seq[ReportCardEntry])] = {
    val validCards = (cards: Seq[ReportCardEntryDb]) => cards.forall(_.assignmentIndex >= 0)

    val result = for {
      shouldContinue <- groupDao.groupHasAtLeastTwoMembers(srcGroup)
      _ <- ensure(shouldContinue, () => "there must be at least one member left after moving")

      maybeDestStudent <- groupDao.firstStudentIn(destGroup)
      destStudent = maybeDestStudent.get
      srcStudentCards <- reportCards(labwork, student).map(_.sortBy(_.assignmentIndex))
      destStudentCards <- reportCards(labwork, destStudent).map(_.sortBy(_.assignmentIndex))
      _ <- ensure(srcStudentCards.size == destStudentCards.size, () => "both src- and dest- reportCardEntries must have the same size")
      removedMembership <- groupDao.remove(student, srcGroup)
      newMembership <- groupDao.add(student, destGroup)

      updatedSrcCards <- if (validCards(destStudentCards) && validCards(srcStudentCards)) {
        val copiedCards = destStudentCards.zip(srcStudentCards)
          .map { case (dest, src) => src.copy(date = dest.date, start = dest.start, end = dest.end, room = dest.room) }

        DBIO.sequence(copiedCards.map(reportCardEntryDao.updateQuery))
      } else
        DBIO.failed(new Throwable("could not copy reportCardEntries because this operation is only supported on cards with a valid assignmentIndex"))
    } yield (removedMembership > 0, newMembership, srcStudentCards.map(_.toUniqueEntity), destStudentCards.map(_.toUniqueEntity), updatedSrcCards.map(_.toUniqueEntity))

    db.run(result.transactionally)
  }

  private def ensure(predicate: Boolean, orMsg: () => String): DBIOAction[Unit, NoStream, Effect] =
    if (predicate) DBIO.successful(()) else DBIO.failed(new Throwable(orMsg()))

  private def reportCards(labwork: UUID, student: UUID): DBIOAction[Seq[ReportCardEntryDb], NoStream, Effect.Read] =
    reportCardEntryDao.withEntryTypes(List(labworkFilter(labwork), userFilter(student)))
}

final class LwmServiceDaoImpl @Inject()(
  val db: Database,
  val executionContext: ExecutionContext,
  val labworkApplicationDao: LabworkApplicationDao,
  val groupDao: GroupDao,
  val reportCardEntryDao: ReportCardEntryDao
) extends LwmServiceDao
