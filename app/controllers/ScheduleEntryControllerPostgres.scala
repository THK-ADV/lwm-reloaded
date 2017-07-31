package controllers

import java.util.UUID

import dao._
import models.Permissions.{schedule, scheduleEntry}
import models._
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.{AnyContent, Request}
import services._
import store.{ScheduleEntryTable, TableFilter}
import utils.{Gen, LwmMimeType}

import scala.concurrent.Future
import scala.util.{Failure, Try}

object ScheduleEntryControllerPostgres {
  lazy val courseAttribute = "course"
  lazy val labworkAttribute = "labwork"
  lazy val groupAttribute = "group"
  lazy val supervisorAttribute = "supervisor"

  lazy val dateAttribute = "date"
  lazy val startAttribute = "start"
  lazy val endAttribute = "end"
  lazy val sinceAttribute = "since"
  lazy val untilAttribute = "until"

  lazy val countAttribute = "count"
  lazy val minAttribute = "min"
  lazy val maxAttribute = "max"

  lazy val popAttribute = "pops"
  lazy val genAttribute = "gens"
  lazy val eliteAttribute = "elites"

  def valueOf(queryString: Map[String, Seq[String]])(attribute: String): Option[String] = {
    queryString.get(attribute).flatMap(_.headOption)
  }

  def intOf(queryString: Map[String, Seq[String]])(attribute: String): Option[Int] = {
    valueOf(queryString)(attribute).flatMap(s => Try(s.toInt).toOption)
  }

  def strategyFrom(queryString: Map[String, Seq[String]]): Try[GroupingStrategy] = {
    val v = valueOf(queryString) _

    (v(countAttribute), v(minAttribute), v(maxAttribute)) match {
      case (Some(count), None, None) =>
        for {
          c <- Try(count.toInt) if c > 0
        } yield CountGrouping(count)
      case (None, Some(min), Some(max)) =>
        for {
          a <- Try(min.toInt)
          b <- Try(max.toInt) if a < b
        } yield RangeGrouping(min, max)
      case _ =>
        Failure(new Exception(s"grouping strategy should be either $countAttribute or $minAttribute and $maxAttribute"))
    }
  }
}

final class ScheduleEntryControllerPostgres(val roleService: RoleServiceLike,
                                            val sessionService: SessionHandlingService,
                                            val abstractDao: ScheduleEntryDao,
                                            val scheduleGenesisService: ScheduleGenesisServiceLike2,
                                            val assignmentPlanService: AssignmentPlanService,
                                            val labworkService: LabworkService,
                                            val timetableService: TimetableService2,
                                            val labworkApplicationService2: LabworkApplicationService2,
                                            val groupDao: GroupDao
                                           ) extends AbstractCRUDControllerPostgres[PostgresScheduleEntryProtocol, ScheduleEntryTable, ScheduleEntryDb, ScheduleEntry] {
  import controllers.ScheduleEntryControllerPostgres._

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[ScheduleEntry] = ScheduleEntry.writes

  override protected implicit val reads: Reads[PostgresScheduleEntryProtocol] = PostgresScheduleEntry.reads

  implicit val scheduleEntryGenWrites = Json.writes[ScheduleEntryGen]

  implicit val scheduleGenWrites = Json.writes[ScheduleGen]

  implicit val conflictWrites = Json.writes[Conflict]

  implicit val genWrites = new Writes[(Gen[ScheduleGen, Conflict, Int], Int)] {
    override def writes(gen: (Gen[ScheduleGen, Conflict, Int], Int)) = Json.obj(
      "schedule" -> Json.toJson(gen._1.elem),
      "conflicts" -> Json.toJson(gen._1.evaluate.err),
      "conflict value" -> gen._1.evaluate.value,
      "fitness" -> gen._2
    )
  }

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[ScheduleEntryTable]]]): Try[List[TableFilter[ScheduleEntryTable]]] = {
    (appendTo, (attribute, value)) match {
      case (list, (`courseAttribute`, course)) => list.map(_.+:(ScheduleEntryCourseFilter(course)))
      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(ScheduleEntryLabworkFilter(labwork)))
      case (list, (`groupAttribute`, group)) => list.map(_.+:(ScheduleEntryGroupFilter(group)))
      case (list, (`supervisorAttribute`, supervisor)) => list.map(_.+:(ScheduleEntrySupervisorFilter(supervisor)))
      case (list, (`dateAttribute`, date)) => list.map(_.+:(ScheduleEntryDateFilter(date)))
      case (list, (`startAttribute`, start)) => list.map(_.+:(ScheduleEntryStartFilter(start)))
      case (list, (`endAttribute`, end)) => list.map(_.+:(ScheduleEntryEndFilter(end)))
      case (list, (`sinceAttribute`, since)) => list.map(_.+:(ScheduleEntrySinceFilter(since)))
      case (list, (`untilAttribute`, until)) => list.map(_.+:(ScheduleEntryUntilFilter(until)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresScheduleEntryProtocol, existingId: Option[UUID]): ScheduleEntryDb = ???

  override implicit val mimeType = LwmMimeType.scheduleEntryV1Json

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, schedule.create) // TODO UNIFY
    case Delete => SecureBlock(restrictionId, schedule.delete)
    case GetAll => SecureBlock(restrictionId, scheduleEntry.getAll)
    case Get => SecureBlock(restrictionId, scheduleEntry.get)
    case Update => SecureBlock(restrictionId, scheduleEntry.update)
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    implicit val readsGroup = Json.reads[PostgresGroup]
    implicit val readsScheduleEntry = Json.reads[ScheduleEntryGen]
    implicit val readsSchedule = Json.reads[ScheduleGen]
    import models.LwmDateTime._

    (for {
      s <- Future.fromTry(parse[ScheduleGen](request))
      labwork = s.labwork
      (se, gs) = s.entries.foldLeft((List.empty[ScheduleEntryDb], Set.empty[GroupDb])) {
        case ((entries, groups), e) =>
          val scheduleEntry = ScheduleEntryDb(labwork, e.start.sqlTime, e.end.sqlTime, e.date.sqlDate, e.room, e.supervisor, e.group.id)
          val group = GroupDb(e.group.label, e.group.labwork, e.group.members)

          (entries.+:(scheduleEntry), groups + group)
      }
      _ <- groupDao.createMany(gs.toList)
      _ <- abstractDao.createMany(se)
    } yield se.map(_.toLwmModel)).jsonResult
  }

  def preview(course: String, labwork: String) = restrictedContext(course)(Create) asyncAction { implicit request =>
    generate(labwork).jsonResult
  }

  private def generate(labwork: String)(implicit request: Request[AnyContent]) = for {
    timetables <- timetableService.withBlacklists(List(TimetableLabworkFilter(labwork))) if timetables.nonEmpty
    (timetable, blacklists) = {
      val h = timetables.head
      (h._1, h._2.toVector)
    }

    applications <- labworkApplicationService2.get(List(LabworkApplicationLabworkFilter(labwork)), atomic = false)
    apps = applications.map(_.asInstanceOf[PostgresLabworkApplication]).toVector

    groupingStrategy <- Future.fromTry(strategyFrom(request.queryString))
    groups = GroupService.groupApplicantsBy(groupingStrategy, apps, UUID.fromString(labwork))

    assignmentPlans <- assignmentPlanService.get(List(AssignmentPlanLabworkFilter(labwork)), atomic = false) if assignmentPlans.nonEmpty
    ap = assignmentPlans.head.asInstanceOf[PostgresAssignmentPlan]

    lab <- labworkService.getById(labwork) if lab.isDefined
    labAtom = lab.get.asInstanceOf[PostgresLabworkAtom]
    semester = labAtom.semester

    comps <- abstractDao.competitive(labAtom)

    i = intOf(request.queryString) _
    pop = i(popAttribute)
    gen = i(genAttribute)
    elite = i(eliteAttribute)
  } yield scheduleGenesisService.generate(timetable, blacklists, groups, ap, semester, comps, pop, gen, elite)

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.append(courseAttribute -> Seq(course)))
  }

  def allFromLabwork(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.append(courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork)))
  }

  def getFrom(course: String, id: String) = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
  }

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncContentTypedAction  { request =>
    update(id, NonSecureBlock)(request)
  }

  def deleteFrom(course: String, labwork: String) = restrictedContext(course)(Delete) asyncAction { request =>
    ???
  }
}
