package controllers

import java.util.UUID

import controllers.helper.{GroupingStrategyAttributeFilter, TimeRangeTableFilter}
import dao._
import dao.helper.TableFilter
import database.{GroupDb, ScheduleEntryDb, ScheduleEntryTable}
import javax.inject.{Inject, Singleton}
import models.Role.{Admin, CourseAssistant, CourseEmployee, CourseManager}
import models._
import models.genesis.{Conflict, ScheduleEntryGen, ScheduleGen}
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
  val assignmentEntryDao: AssignmentEntryDao,
  val labworkDao: LabworkDao,
  val timetableDao: TimetableDao,
  val labworkApplicationDao: LabworkApplicationDao,
  val groupDao: GroupDao,
  val securedAction: SecurityActionChain
) extends AbstractCRUDController[ScheduleEntryProtocol, ScheduleEntryTable, ScheduleEntryDb, ScheduleEntryLike](cc)
  with GroupingStrategyAttributeFilter
  with TimeRangeTableFilter[ScheduleEntryTable] {

  import controllers.ScheduleEntryController._

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
    def groups(entries: Vector[ScheduleEntryGen]) = entries
      .map(_.group)
      .distinct
      .map(g => GroupDb(g.label, g.labwork, g.members, id = g.id))
      .toList

    def scheduleEntries(entries: Vector[ScheduleEntryGen], labwork: UUID) = {
      import utils.date.DateTimeOps.{LocalDateConverter, LocalTimeConverter}

      entries
        .map(e => ScheduleEntryDb(labwork, e.start.sqlTime, e.end.sqlTime, e.date.sqlDate, e.room, e.supervisor, e.group.id))
        .toList
    }

    (for {
      s <- Future.fromTry(parseJson(request)(ScheduleGen.reads))
      gs = groups(s.entries)
      se = scheduleEntries(s.entries, s.labwork)
      _ <- groupDao.createMany(gs)
      _ <- abstractDao.createMany(se)
      atomic = extractAttributes(request.queryString)._2.atomic
      scheduleEntries <- if (atomic)
        abstractDao.getMany(se.map(_.id), atomic)
      else
        Future.successful(se.map(_.toUniqueEntity))
    } yield scheduleEntries).created
  }

  def preview(course: String, labwork: String) = restrictedContext(course)(Create) asyncAction { implicit request =>
    labwork.uuidF.flatMap(l => generate(l)).jsonResult
  }

  private def generate(labwork: UUID)(implicit request: Request[AnyContent]) = {
    def toScheduleGens(xs: Seq[ScheduleEntryLike]) = xs
      .map(_.asInstanceOf[ScheduleEntryAtom])
      .groupBy(_.labworkId)
      .map { case (l, xs) =>
        ScheduleGen(
          l,
          xs.map(x => ScheduleEntryGen(x.start, x.end, x.date, x.room.id, x.supervisor.map(_.id), x.group)).toVector
        )
      }
      .toVector

    val labworkFilter = List(TableFilter.labworkFilter(labwork))

    for {
      groupingStrategy <- Future.fromTry(extractGroupingStrategy(request.queryString))

      (timetable, blacklists) <- timetableDao.withBlacklists(labworkFilter)
      _ = if (timetable.entries.isEmpty) throw new Throwable("timetable entries must be set")

      applications <- labworkApplicationDao.get(labworkFilter, atomic = false)
      _ = if (applications.isEmpty) throw new Throwable("labwork applications must be set")
      apps = applications.map(_.asInstanceOf[LabworkApplication]).toVector

      groups = GroupService.groupApplicantsBy(groupingStrategy)(apps, labwork)

      maybeEntries <- assignmentEntryDao.get(labworkFilter, atomic = false)
      aps = if (maybeEntries.isEmpty) throw new Throwable("assignment entries must be set") else maybeEntries.map(_.asInstanceOf[AssignmentEntry])

      lab <- labworkDao.getSingle(labwork) if lab.isDefined
      labAtom = lab.fold(throw new Throwable("labwork not found"))(_.asInstanceOf[LabworkAtom])
      semester = labAtom.semester

      considerSemesterIndex = boolOf(request.queryString)(semesterIndexConsiderationAttribute).getOrElse(true)
      comps <- abstractDao.competitive(labAtom, atomic = true, considerSemesterIndex = considerSemesterIndex)

      i = intOf(request.queryString) _
      pop = i(popAttribute)
      gen = i(genAttribute)
      elite = i(eliteAttribute)
    } yield scheduleService.generate(
      labwork,
      timetable.entries.toVector,
      timetable.start,
      blacklists.toVector,
      groups,
      aps.toVector,
      semester,
      toScheduleGens(comps),
      pop,
      gen,
      elite
    )
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.appending(courseAttribute -> Seq(course)))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Delete => SecureBlock(restrictionId, List(CourseManager))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee, CourseAssistant))
    case Get => SecureBlock(restrictionId, List(CourseManager, CourseEmployee, CourseAssistant))
    case Update => PartialSecureBlock(List(Admin))
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

  def invalidateFrom(course: String, labwork: String) = restrictedContext(course)(Delete) asyncAction { request =>
    (for {
      labworkId <- labwork.uuidF
      (_, ds) <- ScheduleCombinatorDao.invalidate(labworkId)(abstractDao, groupDao)
    } yield ds).jsonResult
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
