package controllers

import java.util.UUID

import controllers.helper.{JsonParser, ResultOps}
import dao._
import database.helper.LdapUserStatus
import database._
import javax.inject.Inject
import models.{Group, LabworkApplication, ReportCardEntry, ReportCardEvaluation}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{AbstractController, ControllerComponents, Request}
import service.Webservice
import slick.jdbc.JdbcProfile
import utils.date.DateTimeFormatterPattern

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class DevController @Inject()(
  cc: ControllerComponents,
  implicit val ctx: ExecutionContext,
  val profile: JdbcProfile,
  val ws: Webservice,
  val userDao: UserDao,
  val lappDao: LabworkApplicationDao,
  val groupDao: GroupDao,
  val authorityDao: AuthorityDao,
  val reportCardEntryDao: ReportCardEntryDao,
  val reportCardEvaluationDao: ReportCardEvaluationDao
) extends AbstractController(cc)
  with DateTimeFormatterPattern
  with JsonParser
  with ResultOps {

  import profile.api._

  def mergeManyGets[A](request: List[Future[Try[List[A]]]]): Future[List[A]] = {
    import utils.Ops.MonadInstances.tryM
    import utils.Ops._

    Future.sequence(request)
      .map(_.sequence.map(_.flatten))
      .flatMap(Future.fromTry)
  }

  def token(implicit request: Request[JsValue]) = request.body.\("token").as[String]

  def baseUrl() = "http://praktikum.gm.fh-koeln.de:9000"

  def go() = Action.async(parse.json) { implicit request =>
    def toLappDb(app: LabworkApplication) = LabworkApplicationDb(
      app.labwork,
      app.applicant,
      app.friends,
      id = app.id
    )

    def duplicates() = for {
      users <- userDao.get(List(UserDao.statusFilter(LdapUserStatus.StudentStatus)), atomic = false)
      dups = users.groupBy(_.systemId).filter(_._2.size > 1)
    } yield dups

    def deleteApps(origin: UUID, drop: List[UUID]) = for {
      allApps <- lappDao.get(atomic = false).map(_.map(_.asInstanceOf[LabworkApplication]))
      updateToOrigin = allApps
        .filter(a => drop.contains(a.applicant))
        .map(_.copy(applicant = origin))

      updateFriends = allApps
        .filterNot(a => drop.contains(a.applicant)) // alle die nicht zu löschen sind
        .filter(a => a.friends.exists(drop.contains)) // alle wo der gelöschte als freund vorkommt
        .map(a => a.copy(friends = a.friends.filterNot(drop.contains) + origin)) // freund aktualisieren

      _ <- lappDao.updateMany((updateToOrigin ++ updateFriends).map(toLappDb).toList)
    } yield "deleteApps"

    def toGroupDb(g: Group) = GroupDb(g.label, g.labwork, g.members, id = g.id)

    def deleteGroups(origin: UUID, drop: List[UUID]) = for {
      groups <- groupDao.filter(_.containsAny(drop), atomic = false)
      toUpdate = groups
        .map(_.asInstanceOf[Group])
        .map(g => g.copy(members = g.members.filterNot(drop.contains) + origin))
      _ <- groupDao.updateMany(toUpdate.map(toGroupDb).toList)
    } yield "deleteGroups"

    def deleteAuths(drop: List[UUID]) = for {
      auths <- authorityDao.filter(a => a.user.inSet(drop), atomic = false) if auths.forall(_.courseId.isEmpty)
      res <- authorityDao.deleteHard(auths.map(_.id).toList)
    } yield s"deleteAuths $res"

    def toReportCardEntryDb(e: ReportCardEntry) = {
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

    def deleteCards(origin: UUID, drop: List[UUID]) = for {
      dupCards <- reportCardEntryDao.filter(c => c.user.inSet(drop), atomic = false)
      replaced = dupCards
        .map(_.asInstanceOf[ReportCardEntry])
        .map(e => e.copy(student = origin))
      _ <- reportCardEntryDao.updateMany(replaced.map(toReportCardEntryDb).toList)
    } yield "deleteCards"

    def toReportCardEvalDb(e: ReportCardEvaluation) = ReportCardEvaluationDb(
      e.student, e.labwork, e.label, e.bool, e.int, id = e.id
    )

    def deleteEvals(origin: UUID, drop: List[UUID]) = for {
      dupEvals <- reportCardEvaluationDao.filter(c => c.user.inSet(drop), atomic = false)
      replaced = dupEvals
        .map(_.asInstanceOf[ReportCardEvaluation])
        .map(e => e.copy(student = origin))
      _ <- reportCardEvaluationDao.updateMany(replaced.map(toReportCardEvalDb).toList)
    } yield "deleteEvals"

    def deleteStudents(drop: List[UUID]) = {
      for {
        delete <- userDao.filter(u => u.id.inSet(drop), atomic = false)
        res <- userDao.deleteHard(delete.map(_.id).toList)
      } yield s"deleteStudents $res"
    }

    for {
      dups <- duplicates()
      pairs = dups.map(t => (t._2.head.id, t._2.tail.map(_.id).toList)).toList
      res <- Future.sequence(pairs.map {
        case (origin, drop) =>
          for {
            deleteApps <- deleteApps(origin, drop)
            deleteGroups <- deleteGroups(origin, drop)
            deleteCards <- deleteCards(origin, drop)
            deleteEvals <- deleteEvals(origin, drop)
            deleteAuths <- deleteAuths(drop)
            deleteStudents <- deleteStudents(drop)
          } yield List(deleteApps, deleteGroups, deleteCards, deleteEvals, deleteAuths, deleteStudents)
      })
    } yield Ok(Json.toJson(res))
  }
}
