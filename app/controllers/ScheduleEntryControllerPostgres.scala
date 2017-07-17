package controllers

import java.util.UUID

import models.Permissions.{schedule, scheduleEntry}
import models._
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.{AnyContent, Request}
import services._
import store.{ScheduleEntryTable, TableFilter}
import utils.{Gen, LwmMimeType}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object ScheduleEntryControllerPostgres {
  lazy val courseAttribute = "course"
  lazy val labworkAttribute = "labwork"
  /*val groupAttribute = "group"
  val supervisorAttribute = "supervisor"
  val dateAttribute = "date"
  val startAttribute = "start"
  val endAttribute = "end"
  val dateRangeAttribute = "dateRange"*/

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
                                            val labworkApplicationService2: LabworkApplicationService2
                                           ) extends AbstractCRUDControllerPostgres[PostgresScheduleEntryProtocol, ScheduleEntryTable, ScheduleEntryDb, ScheduleEntry] {
  import controllers.ScheduleEntryControllerPostgres._
  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[ScheduleEntry] = ???

  override protected implicit val reads: Reads[PostgresScheduleEntryProtocol] = ???

  implicit val scheduleEntryGenWrties = Json.writes[ScheduleEntryGen]

  implicit val scheduleGenWrties = Json.writes[ScheduleGen]

  implicit val conflictWrites = Json.writes[Conflict]

  implicit val genWrites = new Writes[(Gen[ScheduleGen, Conflict, Int], Int)] {
    override def writes(gen: (Gen[ScheduleGen, Conflict, Int], Int)) = Json.obj(
      "schedule" -> Json.toJson(gen._1.elem),
      "conflicts" -> Json.toJson(gen._1.evaluate.err),
      "conflict value" -> gen._1.evaluate.value,
      "fitness" -> gen._2
    )
  }

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[ScheduleEntryTable]]]): Try[List[TableFilter[ScheduleEntryTable]]] = ???

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
    ???
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
