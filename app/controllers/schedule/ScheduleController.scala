package controllers.schedule

import java.util.UUID

import controllers.crud._
import models.labwork._
import models.security.Permissions.{god, schedule}
import models.UriGenerator
import modules.store.BaseNamespace
import org.openrdf.model.Value
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import services._
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import store.bind.Descriptor.Descriptor
import store.{Namespace, SesameRepository}
import utils.{Attempt, Gen, LwmMimeType}
import ScheduleController._

import scala.util.Try

object ScheduleController {

  def competitive(labwork: UUID, repository: SesameRepository): Try[Set[ScheduleG]] = {
    scheduleFor(labwork, repository) map { set =>
      set.flatMap(schedule => toScheduleG(schedule, repository))
    }
  }

  private def scheduleFor(labwork: UUID, repository: SesameRepository): Try[Set[Schedule]] = {
    lazy val lwm = LWMPrefix[repository.Rdf]
    val bindings = Bindings[repository.Rdf](repository.namespace)

    import bindings.ScheduleDescriptor
    import store.sparql.select
    import store.sparql.select._
    import utils.Ops._
    import MonadInstances.listM

    lazy val id = Labwork.generateUri(labwork)(repository.namespace)

    val query = select distinct "schedules" where {
      **(s(id), p(lwm.course), v("courseid")).
        **(s(id), p(lwm.semester), v("semester")).
        **(v("course"), p(lwm.id), v("courseid")).
        **(v("course"), p(lwm.semesterIndex), v("semesterIndex")).
        **(v("labwork"), p(lwm.semester), v("semester")).
        **(v("labwork"), p(lwm.course), v("course2id")).
        **(v("course2"), p(lwm.id), v("course2id")).
        **(v("course2"), p(lwm.semesterIndex), v("semesterIndex")).
        **(v("labwork"), p(lwm.id), v("labworkid")).
        **(v("schedules"), p(lwm.labwork), v("labworkid"))
    }

    repository.prepareQuery(query).
      select(_.get("schedules")).
      transform(_.fold(List.empty[Value])(identity)).
      map(_.stringValue()).
      requestAll(repository.getMany[Schedule]).
      run
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
    Schedule(scheduleG.labwork, entries, scheduleG.id)
  }
}

class ScheduleController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService, val scheduleGenesisService: ScheduleGenesisServiceLike)
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
    case _ => PartialSecureBlock(god)
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
    val url = (UUID.fromString _ andThen Schedule.generateUri) (schedule)
    remove[Schedule](url)
      .mapResult(_ => Ok(Json.obj("status" -> "OK")))
  }

  def preview(course: String, labwork: String) = restrictedContext(course)(Create) action { request =>
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

  def generate(labwork: String): Attempt[Gen[ScheduleG, Conflict, Int]] = {
    import ScheduleController._
    import defaultBindings.{GroupDescriptor, LabworkAtomDescriptor, TimetableDescriptor, AssignmentPlanDescriptor}

    val id = UUID.fromString(labwork)

    val genesis = for {
      lab <- repository.get[LabworkAtom](Labwork.generateUri(UUID.fromString(labwork)))
      semester = lab map (_.semester)
      groups <- repository.getAll[Group].map(_.filter(_.labwork == id))
      timetable <- repository.getAll[Timetable].map(_.find(_.labwork == id))
      plans <- repository.getAll[AssignmentPlan].map(_.find(_.labwork == id))
      comp <- competitive(id, repository)
    } yield for {
      t <- timetable if t.entries.nonEmpty
      p <- plans if p.entries.nonEmpty
      s <- semester
      g <- if (groups.nonEmpty) Some(groups) else None
    } yield scheduleGenesisService.generate(t, g, p, s, comp.toVector)._1

    optional(genesis)
  }

}