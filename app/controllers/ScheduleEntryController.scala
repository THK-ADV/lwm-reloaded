package controllers

import java.util.UUID

import controllers.helper.{GroupingStrategyAttributeFilter, TimeRangeTableFilter}
import dao._
import database.{GroupDb, ScheduleEntryDb, ScheduleEntryTable}
import javax.inject.{Inject, Singleton}
import models.Role.{CourseAssistant, CourseEmployee, CourseManager}
import models._
import models.genesis.{Conflict, ScheduleGen}
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.{AnyContent, ControllerComponents, Request}
import security.SecurityActionChain
import service._
import utils.Gen

import scala.concurrent.Future
import scala.util.Try

object ScheduleEntryController {
  lazy val courseAttribute = "course"
  lazy val labworkAttribute = "labwork"
  lazy val groupAttribute = "group"
  lazy val supervisorAttribute = "supervisor"

  lazy val popAttribute = "pops"
  lazy val genAttribute = "gens"
  lazy val eliteAttribute = "elites"

  lazy val semesterIndexConsiderationAttribute = "considerSemesterIndex"
}

@Singleton
final class ScheduleEntryController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val abstractDao: ScheduleEntryDao,
  val scheduleService: ScheduleService,
  val assignmentPlanService: AssignmentPlanDao,
  val labworkService: LabworkDao,
  val timetableService: TimetableDao,
  val labworkApplicationService2: LabworkApplicationDao,
  val groupDao: GroupDao,
  val securedAction: SecurityActionChain
) extends AbstractCRUDController[ScheduleEntryProtocol, ScheduleEntryTable, ScheduleEntryDb, ScheduleEntryLike](cc) with GroupingStrategyAttributeFilter
  with TimeRangeTableFilter[ScheduleEntryTable] {

  import controllers.ScheduleEntryController._
  import dao.helper.TableFilter.labworkFilter

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[ScheduleEntryLike] = ScheduleEntryLike.writes

  override protected implicit val reads: Reads[ScheduleEntryProtocol] = ScheduleEntryProtocol.reads

  implicit val genWrites: Writes[(Gen[ScheduleGen, Conflict, Int], Int)] = new Writes[(Gen[ScheduleGen, Conflict, Int], Int)] {
    override def writes(gen: (Gen[ScheduleGen, Conflict, Int], Int)) = Json.obj(
      "schedule" -> Json.toJson(gen._1.elem),
      "conflicts" -> Json.toJson(gen._1.evaluate.err),
      "conflict value" -> gen._1.evaluate.value,
      "fitness" -> gen._2
    )
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncAction { implicit request =>
    import utils.date.DateTimeOps.{LocalDateConverter, LocalTimeConverter}

    (for {
      s <- Future.fromTry(parseJson(request)(ScheduleGen.reads))
      labwork = s.labwork
      groups = s.entries.map(_.group).distinct.map(e => GroupDb(e.label, e.labwork, e.members, id = e.id)).toList
      se = s.entries.map(e => database.ScheduleEntryDb(labwork, e.start.sqlTime, e.end.sqlTime, e.date.sqlDate, e.room, e.supervisor, e.group.id)).toList
      _ <- groupDao.createMany(groups)
      _ <- abstractDao.createMany(se)

      atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic
      scheduleEntries <- if (atomic)
        abstractDao.getMany(se.map(_.id), atomic)
      else
        Future.successful(se.map(_.toUniqueEntity))

    } yield scheduleEntries).jsonResult
  }

  def preview(course: String, labwork: String) = restrictedContext(course)(Create) asyncAction { implicit request =>
    labwork.uuidF.flatMap(l => generate(l)).created
  }

  private def generate(labwork: UUID)(implicit request: Request[AnyContent]) = for {
    timetables <- timetableService.withBlacklists(List(labworkFilter(labwork))) if timetables.nonEmpty
    (timetable, blacklists) = {
      val h = timetables.head
      (h._1, h._2.toVector)
    }

    applications <- labworkApplicationService2.get(List(labworkFilter(labwork)), atomic = false)
    apps = applications.map(_.asInstanceOf[LabworkApplication]).toVector

    groupingStrategy <- Future.fromTry(extractGroupingStrategy(request.queryString))
    groups = GroupService.groupApplicantsBy(groupingStrategy, apps, labwork)

    assignmentPlans <- assignmentPlanService.get(List(labworkFilter(labwork)), atomic = false) if assignmentPlans.nonEmpty
    ap = assignmentPlans.head.asInstanceOf[AssignmentPlan]

    lab <- labworkService.getSingle(labwork) if lab.isDefined
    labAtom = lab.get.asInstanceOf[LabworkAtom]
    semester = labAtom.semester

    c = boolOf(request.queryString)(semesterIndexConsiderationAttribute).getOrElse(true)
    comps <- abstractDao.competitive(labAtom, c)

    i = intOf(request.queryString) _
    pop = i(popAttribute)
    gen = i(genAttribute)
    elite = i(eliteAttribute)
  } yield scheduleService.generate(timetable, blacklists, groups, ap, semester, comps, pop, gen, elite)

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.appending(courseAttribute -> Seq(course)))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Delete => SecureBlock(restrictionId, List(CourseManager))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee, CourseAssistant))
    case Get => SecureBlock(restrictionId, List(CourseManager, CourseEmployee, CourseAssistant))
    case Update => SecureBlock(restrictionId, List(CourseManager))
  }

  def allFromLabwork(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.appending(courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork)))
  }

  def getFrom(course: String, id: String) = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
  }

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncAction { request =>
    update(id, NonSecureBlock)(request)
  }

  def deleteFrom(course: String, labwork: String) = restrictedContext(course)(Delete) asyncAction { request =>
    ???
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import ScheduleEntryController._
    import dao.ScheduleEntryDao.supervisorFilter

    (attribute, value) match {
      case (`courseAttribute`, c) => c.makeCourseFilter
      case (`labworkAttribute`, l) => l.makeLabworkFilter
      case (`groupAttribute`, g) => g.makeGroupFilter
      case (`supervisorAttribute`, s) => s.uuid map supervisorFilter
      case _ => makeTimeRangeFilter(attribute, value)
    }
  }

  override protected def toDbModel(protocol: ScheduleEntryProtocol, existingId: Option[UUID]): ScheduleEntryDb = ???

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbiddenAction()
}
