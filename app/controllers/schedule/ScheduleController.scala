package controllers.schedule

import java.util.UUID

import controllers.crud._
import models.labwork._
import models.security.Permissions.{god, prime, schedule}
import models.UriGenerator
import modules.store.BaseNamespace
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.{Action, Controller, Request}
import services._
import store.bind.Bindings
import store.bind.Descriptor.Descriptor
import store.{Namespace, SesameRepository}
import utils.{Attempt, Gen, LwmMimeType}

import scala.util.{Failure, Success, Try}

object ScheduleController {

  def toScheduleG(schedule: Schedule, repository: SesameRepository): Option[ScheduleG] = {
    val bindings = Bindings[repository.Rdf](repository.namespace)
    import bindings.GroupDescriptor
    import utils.Ops._
    import MonadInstances._

    val maybeEntries =
      (schedule.entries flatMap { entry =>
        val group = repository.get[Group](Group.generateUri(entry.group)(repository.namespace)).toOption
        group.peek(g => ScheduleEntryG(entry.start, entry.end, entry.date, entry.room, entry.supervisor, g))
      }).sequence


    maybeEntries map (entries => ScheduleG(schedule.labwork, entries.toVector, schedule.id))
  }

  def toSchedule(scheduleG: ScheduleG): Schedule = {
    val entries = scheduleG.entries.map(e => ScheduleEntry(scheduleG.labwork, e.start, e.end, e.date, e.room, e.supervisor, e.group.id)).toSet
    Schedule(scheduleG.labwork, entries, None, scheduleG.id)
  }

  val popAttribute = "pops"
  val genAttribute = "gens"
  val eliteAttribute = "elites"
}

// TODO inherit from AbstractCRUDController
class ScheduleController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService, val scheduleGenesisService: ScheduleGenesisServiceLike, val groupService: GroupServiceLike)
  extends Controller
    with BaseNamespace
    with ContentTyped
    with Secured
    with SessionChecking
    with SecureControllerContext
    with Stored
    with JsonSerialisation[Schedule, Schedule, ScheduleAtom]
    with RdfSerialisation[Schedule, ScheduleAtom]
    with Removed
    with Added[Schedule, Schedule, ScheduleAtom]
    with Retrieved[Schedule, ScheduleAtom] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.scheduleV1Json

  override implicit val descriptor: Descriptor[Sesame, Schedule] = defaultBindings.ScheduleDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, ScheduleAtom] = defaultBindings.ScheduleAtomDescriptor

  override implicit val reads: Reads[Schedule] = Schedule.reads

  override implicit val writes: Writes[Schedule] = Schedule.writes

  override implicit val writesAtom: Writes[ScheduleAtom] = Schedule.writesAtom

  override implicit val uriGenerator: UriGenerator[Schedule] = Schedule

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, schedule.create)
    case Delete => SecureBlock(restrictionId, schedule.delete)
    case Get => PartialSecureBlock(prime)
    case _ => PartialSecureBlock(god)
  }

  // TODO proper implementation
  def get(course: String, labwork: String) = restrictedContext(course)(Get) action { request =>
    repository.getAll[Schedule].map(_.find(_.labwork == UUID.fromString(labwork))) match {
      case Success(Some(s)) => Ok(Json.toJson(s)).as(mimeType)
      case Success(None) => NotFound(Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      ))
      case Failure(e) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }

  def create(course: String) = restrictedContext(course)(Create) contentTypedAction { request =>
    validate(request)
      .flatMap(add)
      .mapResult(s => Created(Json.toJson(s)).as(mimeType))
  }

  def createAtomic(course: String) = restrictedContext(course)(Create) contentTypedAction { request =>
    validate(request)
      .flatMap(add)
      .flatMap(s => retrieve[ScheduleAtom](Schedule.generateUri(s)))
      .mapResult(s => Created(Json.toJson(s)).as(mimeType))
  }

  def delete(course: String, schedule: String) = restrictedContext(course)(Delete) action { request =>
    val uri = (UUID.fromString _ andThen Schedule.generateUri) (schedule)
    remove[Schedule](uri)
      .mapResult(_ => Ok(Json.obj("status" -> "OK")))
  }

  def preview(course: String, labwork: String) = restrictedContext(course)(Create) action { implicit request =>
    import controllers.schedule.ScheduleController.toSchedule
    import controllers.crud.labwork.GroupCRUDController.fromQueryString

    optional2(fromQueryString(request.queryString))
      .flatMap(strategy => generate(labwork, strategy))
      .map(_ map toSchedule)
      .mapResult { gen =>
        Ok(Json.obj(
          "status" -> "OK",
          "schedule" -> Json.toJson(gen.elem),
          "number of conflicts" -> gen.evaluate.err.size // TODO serialize conflicts
        ))
      }
  }

  def previewAtomic(course: String, labwork: String) = restrictedContext(course)(Create) action { implicit request =>
    import controllers.schedule.ScheduleController.toSchedule
    import controllers.crud.labwork.GroupCRUDController.fromQueryString

    optional2(fromQueryString(request.queryString))
      .flatMap(strategy => generate(labwork, strategy))
      .map(_ map toSchedule)
      .flatMap { gen =>
        retrieve[ScheduleAtom](Schedule.generateUri(gen.elem))
          .map(atom => gen.map(_ => atom))
      }
      .mapResult { gen =>
        Ok(Json.obj(
          "status" -> "OK",
          "schedule" -> Json.toJson(gen.elem),
          "number of conflicts" -> gen.evaluate.err.size // TODO serialize conflicts
        ))
      }
  }

  def header = Action { implicit request =>
    NoContent.as(mimeType)
  }

  private def generate[A](labwork: String, groupStrategy: Strategy)(implicit request: Request[A]): Attempt[Gen[ScheduleG, Conflict, Int]] = {
    import controllers.schedule.ScheduleController._
    import defaultBindings.{LabworkAtomDescriptor, TimetableDescriptor, AssignmentPlanDescriptor}

    def extract(query: Map[String, Seq[String]])(key: String) = query.get(key).flatMap(_.headOption).map(_.toInt)

    val labId = UUID.fromString(labwork)

    val genesis = for {
      lab <- repository.get[LabworkAtom](Labwork.generateUri(labId))
      semester = lab map (_.semester)
      groups <- groupService.groupBy(labId, groupStrategy)
      timetable <- repository.getAll[Timetable].map(_.find(_.labwork == labId))
      plans <- repository.getAll[AssignmentPlan].map(_.find(_.labwork == labId))
      all <- repository.getAll[ScheduleAtom]
      comp = scheduleGenesisService.competitive(lab, all)
      valueOf = extract(request.queryString) _
      pop = valueOf(popAttribute)
      gen = valueOf(genAttribute)
      elite = valueOf(eliteAttribute)
    } yield for {
      t <- timetable if t.entries.nonEmpty
      p <- plans if p.entries.nonEmpty
      s <- semester
      g <- if (groups.nonEmpty) Some(groups) else None
    } yield {
      comp.foreach(s => println(s"comp :: ${s.labwork}"))
      val genesis = scheduleGenesisService.generate(t, g, p, s, comp.toVector, pop, gen, elite)
      val newGroups = genesis._1.elem.entries.map(_.group)
      printGroup(groups, newGroups)

      println(s"final conflicts ${genesis._1.evaluate.err.size} :: fitness ${genesis._2}")
      genesis._1
    }

    optional(genesis)
  }

  def printGroup(g1: Set[Group], g2: Vector[Group]) {
    g1.toVector.sortBy(_.label).zip(g2.distinct.sortBy(_.label)).foreach {
      case (l, r) =>
        println(s" ${l.label} :: ${r.label} :: ${l.id == r.id}")
        l.members.toVector.sorted zip r.members.toVector.sorted foreach (t => println(s"${t._1} :: ${t._2} :: ${t._1 == t._2}"))
        println("=========")
    }
  }
}