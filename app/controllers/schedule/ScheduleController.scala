package controllers.schedule

import java.util.UUID

import controllers.crud._
import models.labwork._
import models.security.Permissions.{god, prime, schedule}
import models.UriGenerator
import modules.store.BaseNamespace
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import services._
import store.bind.Bindings
import store.bind.Descriptor.Descriptor
import store.{Namespace, SesameRepository}
import utils.{Attempt, Gen, LwmMimeType}

import scala.util.{Failure, Success, Try}

object ScheduleController {

  def competitive(labwork: UUID, repository: SesameRepository): Try[Set[ScheduleG]] = {

    implicit val ns = repository.namespace
    val bindings = Bindings[repository.Rdf](repository.namespace)

    import bindings.{LabworkAtomDescriptor, ScheduleAtomDescriptor}

    for {
      all <- repository.getAll[ScheduleAtom]
      labwork <- repository.get[LabworkAtom](Labwork.generateUri(labwork))
      res = labwork.fold(Set.empty[ScheduleAtom]) { item =>
        all
          .filter(_.labwork.course.semesterIndex == item.course.semesterIndex)
          .filter(_.labwork.semester.id == item.semester.id)
          .filter(_.labwork.degree.id == item.degree.id)
            .filterNot(_.labwork.id == item.id)
      }
    } yield res map { atom =>
      ScheduleG(atom.labwork.id,
        atom.entries.map(e =>
          ScheduleEntryG(
            e.start,
            e.end,
            e.date,
            e.room.id,
            e.supervisor map (_.id),
            e.group)).toVector
        , atom.id)
    }
  }

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

  def preview(course: String, labwork: String) = restrictedContext(course)(Create) action { request =>
    import controllers.schedule.ScheduleController.toSchedule

    generate(labwork)
      .map(_ map toSchedule)
      .mapResult { gen =>
        Ok(Json.obj(
          "status" -> "OK",
          "schedule" -> Json.toJson(gen.elem),
          "number of conflicts" -> gen.evaluate.err.size // TODO serialize conflicts
        ))
      }
  }

  def previewAtomic(course: String, labwork: String) = restrictedContext(course)(Create) action { request =>
    import controllers.schedule.ScheduleController.toSchedule

    generate(labwork)
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

  private def generate(labwork: String, strategy: Strategy): Attempt[Gen[ScheduleG, Conflict, Int]] = {
    import controllers.schedule.ScheduleController._
    import defaultBindings.{LabworkAtomDescriptor, TimetableDescriptor, AssignmentPlanDescriptor}

    val labId = UUID.fromString(labwork)

    val genesis = for {
      lab <- repository.get[LabworkAtom](Labwork.generateUri(labId))
      semester = lab map (_.semester)
      groups <- groupService.groupBy(labId, strategy)
      timetable <- repository.getAll[Timetable].map(_.find(_.labwork == labId))
      plans <- repository.getAll[AssignmentPlan].map(_.find(_.labwork == labId))
      comp <- competitive(labId, repository)
    } yield for {
      t <- timetable if t.entries.nonEmpty
      p <- plans if p.entries.nonEmpty
      s <- semester
      g <- if (groups.nonEmpty) Some(groups) else None
    } yield {
      comp.foreach(s => println(s.labwork))
      val gen = scheduleGenesisService.generate(t, g, p, s, comp.toVector)
      val newGroups = gen._1.elem.entries.map(_.group)
      printGroup(groups, newGroups)

      println(s"final ${gen._1.evaluate.err.size} :: ${gen._2}")
      gen._1
    }

    optional(genesis)
  }

  def printGroup(g1: Set[Group], g2: Vector[Group]): Unit = {
    g1.toVector.sortBy(_.label).zip(g2.distinct.sortBy(_.label)).foreach {
      case (l, r) =>
        println(s" ${l.label} :: ${r.label} :: ${l.id == r.id}")
        l.members.toVector.sorted zip r.members.toVector.sorted foreach (t => println(s"${t._1} :: ${t._2} :: ${t._1 == t._2}"))
        println("=========")
    }
  }
}