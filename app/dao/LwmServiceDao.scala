package dao

import dao.helper.{Core, TableFilter}
import database._
import database.helper.LdapUserStatus
import models._
import slick.jdbc.PostgresProfile.api._

import java.util.UUID
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

trait LwmServiceDao extends Core {

  protected def labworkApplicationDao: LabworkApplicationDao

  protected def groupDao: GroupDao

  protected def reportCardEntryDao: ReportCardEntryDao

  def insertStudentToGroup(student: UUID, labwork: UUID, group: UUID): Future[(GroupMembership, LabworkApplication, Seq[ReportCardEntry], Option[UUID])]

  def removeStudentFromGroup(student: UUID, labwork: UUID, group: UUID): Future[(Boolean, LabworkApplication, Seq[ReportCardEntry])]

  def moveStudentToGroup(student: UUID, labwork: UUID, srcGroup: UUID, destGroup: UUID): Future[(Boolean, GroupMembership, Seq[ReportCardEntry], Seq[ReportCardEntry], Seq[ReportCardEntry])]

  def mergeUser(originSystemId: String, dropSystemId: String): Future[List[String]]

  def duplicateStudents(): Future[Map[String, Seq[Student]]]

  def usersWithoutRegistrationId(): Future[Seq[Student]]
}

final class LwmServiceDaoImpl @Inject()(
  val db: Database,
  val executionContext: ExecutionContext,
  val labworkApplicationDao: LabworkApplicationDao,
  val groupDao: GroupDao,
  val reportCardEntryDao: ReportCardEntryDao,
  val userDao: UserDao,
  val lappDao: LabworkApplicationDao,
  val authorityDao: AuthorityDao,
  val reportCardEvaluationDao: ReportCardEvaluationDao,
  implicit val ctx: ExecutionContext
) extends LwmServiceDao {

  import TableFilter.{labworkFilter, userFilter}
  import utils.Ops.unwrap

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

  def mergeUser(originSystemId: String, dropSystemId: String): Future[List[String]] =
    for {
      originUser <- getUser(originSystemId)
      dropUser <- getUser(dropSystemId)
      origin = originUser.id
      drop = List(dropUser.id)
      res <- for {
        deleteApps <- mergeApps(origin, drop)
        deleteGroups <- mergeGroups(origin, drop)
        deleteCards <- mergeReportCards(origin, drop)
        deleteEvals <- mergeEvals(origin, drop)
        deleteAuths <- deleteAuths(drop)
        deleteStudents <- deleteStudents(drop)
      } yield List(deleteApps, deleteGroups, deleteCards, deleteEvals, deleteAuths, deleteStudents)
    } yield res

  override def duplicateStudents(): Future[Map[String, Seq[Student]]] = for {
    users <- userDao.get(List(UserDao.statusFilter(LdapUserStatus.StudentStatus)), atomic = false)
    dups = users
      .map(_.asInstanceOf[Student])
      .groupBy(a => a.registrationId)
      .filter(a => validRegistrationId(a._1) && a._2.size > 1)
  } yield dups

  override def usersWithoutRegistrationId(): Future[Seq[Student]] = for {
    users <- userDao.get(List(UserDao.statusFilter(LdapUserStatus.StudentStatus)), atomic = false)
  } yield users
    .map(_.asInstanceOf[Student])
    .filterNot(a => validRegistrationId(a.registrationId))

  private def toLappDb(app: LabworkApplication) = LabworkApplicationDb(
    app.labwork,
    app.applicant,
    app.friends,
    id = app.id
  )

  private def validRegistrationId(id: String) =
    id.nonEmpty && id != "0000000000"

  private def toGroupDb(g: Group) = GroupDb(g.label, g.labwork, g.members, id = g.id)

  private def toReportCardEntryDb(e: ReportCardEntry) = {
    import utils.date.DateTimeOps._

    ReportCardEntryDb(
      e.student,
      e.labwork,
      e.label,
      e.date.sqlDate,
      e.start.sqlTime,
      e.end.sqlTime,
      e.room,
      e.entryTypes.map(t =>
        ReportCardEntryTypeDb(Some(e.id), None, t.entryType, t.bool, t.int, id = t.id)
      ),
      e.assignmentIndex,
      e.rescheduled.map(rs =>
        ReportCardRescheduledDb(e.id, rs.date.sqlDate, rs.start.sqlTime, rs.end.sqlTime, rs.room, rs.reason, id = rs.id)
      ),
      e.retry.map(rt =>
        ReportCardRetryDb(e.id, rt.date.sqlDate, rt.start.sqlTime, rt.end.sqlTime, rt.room, rt.entryTypes.map(t =>
          ReportCardEntryTypeDb(None, Some(rt.id), t.entryType, t.bool, t.int, id = t.id)
        ), rt.reason, id = rt.id)
      ),
      id = e.id
    )
  }

  private def toReportCardEvalDb(e: ReportCardEvaluation) = ReportCardEvaluationDb(
    e.student, e.labwork, e.label, e.bool, e.int, id = e.id
  )

  private def mergeApps(origin: UUID, drop: List[UUID]) = for {
    allApps <- lappDao.get(atomic = false).map(_.map(_.asInstanceOf[LabworkApplication]))
    updateToOrigin = allApps
      .filter(a => drop.contains(a.applicant))
      .map(_.copy(applicant = origin))

    updateFriends = allApps
      .filterNot(a => drop.contains(a.applicant)) // alle die nicht zu löschen sind
      .filter(a => a.friends.exists(drop.contains)) // alle wo der gelöschte als freund vorkommt
      .map(a => a.copy(friends = a.friends.filterNot(drop.contains) + origin)) // freund aktualisieren

    _ <- lappDao.updateMany((updateToOrigin ++ updateFriends).map(toLappDb).toList)
  } yield "mergeApps"

  private def mergeGroups(origin: UUID, drop: List[UUID]) = for {
    groups <- groupDao.filter(_.containsAny(drop), atomic = false)
    toUpdate = groups
      .map(_.asInstanceOf[Group])
      .map(g => g.copy(members = g.members.filterNot(drop.contains) + origin))
    _ <- groupDao.updateMany(toUpdate.map(toGroupDb).toList)
  } yield "mergeGroups"

  private def deleteAuths(drop: List[UUID]) = for {
    auths <- authorityDao.filter(a => a.user.inSet(drop), atomic = false) if auths.forall(_.courseId.isEmpty)
    res <- authorityDao.deleteHard(auths.map(_.id).toList)
  } yield s"deleteAuths $res"

  private def mergeReportCards(origin: UUID, drop: List[UUID]) = for {
    dupCards <- reportCardEntryDao.filter(c => c.user.inSet(drop), atomic = false)
    replaced = dupCards
      .map(_.asInstanceOf[ReportCardEntry])
      .map(e => e.copy(student = origin))
    _ <- reportCardEntryDao.updateMany(replaced.map(toReportCardEntryDb).toList)
  } yield "mergeReportCards"

  private def mergeEvals(origin: UUID, drop: List[UUID]) = for {
    dupEvals <- reportCardEvaluationDao.filter(c => c.user.inSet(drop), atomic = false)
    replaced = dupEvals
      .map(_.asInstanceOf[ReportCardEvaluation])
      .map(e => e.copy(student = origin))
    _ <- reportCardEvaluationDao.updateMany(replaced.map(toReportCardEvalDb).toList)
  } yield "mergeEvals"

  private def deleteStudents(drop: List[UUID]) = for {
    delete <- userDao.filter(u => u.id.inSet(drop), atomic = false)
    res <- userDao.deleteHard(delete.map(_.id).toList)
  } yield s"deleteStudents $res"

  private def getUser(systemId: String) =
    unwrap(userDao.getBySystemId(systemId, atomic = false), () => s"no user found for systemId $systemId")
}
