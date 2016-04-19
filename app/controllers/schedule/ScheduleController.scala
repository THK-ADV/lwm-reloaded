package controllers.schedule

import java.util.UUID

import controllers.crud._
import models.labwork._
import models.security.Permissions.{god, schedule}
import models.semester.Semester
import models.users.{Employee, User}
import models.{Room, UriGenerator}
import modules.store.BaseNamespace
import org.openrdf.model.Value
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.{Action, Controller, Result}
import services._
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import store.{Namespace, SesameRepository}
import utils.{Gen, LwmMimeType}

import scala.util.{Failure, Success, Try}

object ScheduleController {

  def competitive(labwork: UUID, repository: SesameRepository): Try[Set[ScheduleG]] = {
    scheduleFor(labwork, repository) map { set =>
      set.flatMap(schedule => toScheduleG(schedule, repository))
    }
  }

  private def scheduleFor(labwork: UUID, repository: SesameRepository): Try[Set[Schedule]] = {
    lazy val lwm = LWMPrefix[repository.Rdf]
    val bindings = Bindings[repository.Rdf](repository.namespace)

    import bindings.ScheduleBinding._
    import store.sparql.select
    import store.sparql.select._
    import utils.Ops._
    import MonadInstances.listM

    lazy val id = Labwork.generateUri(labwork)(repository.namespace)

    val query = select distinct "schedules" where {
      ^(s(id), p(lwm.course), v("courseid")).
        ^(s(id), p(lwm.semester), v("semester")).
        ^(v("course"), p(lwm.id), v("courseid")).
        ^(v("course"), p(lwm.semesterIndex), v("semesterIndex")).
        ^(v("labwork"), p(lwm.semester), v("semester")).
        ^(v("labwork"), p(lwm.course), v("course2id")).
        ^(v("course2"), p(lwm.id), v("course2id")).
        ^(v("course2"), p(lwm.semesterIndex), v("semesterIndex")).
        ^(v("labwork"), p(lwm.id), v("labworkid")).
        ^(v("schedules"), p(lwm.labwork), v("labworkid"))
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
    import bindings.GroupBinding.groupBinder
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
    Schedule(scheduleG.labwork, entries, published = false, scheduleG.id)
  }
}

class ScheduleController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService, val scheduleGenesisService: ScheduleGenesisServiceLike)
  extends Controller
    with BaseNamespace
    with ContentTyped
    with Secured
    with SessionChecking
    with SecureControllerContext
    with SesameRdfSerialisation[Schedule]
    with JsonSerialisation[Schedule, Schedule]
    with Atomic[Schedule] {

  override implicit def rdfWrites: ToPG[Sesame, Schedule] = defaultBindings.ScheduleBinding.scheduleBinder

  override implicit def rdfReads: FromPG[Sesame, Schedule] = defaultBindings.ScheduleBinding.scheduleBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Schedule] = defaultBindings.ScheduleBinding.classUri

  override implicit def uriGenerator: UriGenerator[Schedule] = Schedule

  override implicit def reads: Reads[Schedule] = Schedule.reads

  override implicit def writes: Writes[Schedule] = Schedule.writes

  override implicit val mimeType: LwmMimeType = LwmMimeType.scheduleV1Json

  def create(course: String) = createWith(course) { s =>
    Success(Created(Json.toJson(s)).as(mimeType))
  }

  def createAtomic(course: String) = createWith(course) { s =>
    atomizeMany(Set(s)).map(json => Created(json).as(mimeType))
  }

  def delete(course: String, schedule: String) = restrictedContext(course)(Delete) action { implicit request =>
    repository.deleteCascading((UUID.fromString _ andThen Schedule.generateUri)(schedule)) match {
      case Success(s) =>
        Ok(Json.obj(
          "status" -> "OK",
          "deleted" -> s
        ))
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  def preview(course: String, labwork: String) = previewWith(course, labwork) { gen =>
    Success(Some(Ok(Json.obj(
      "status" -> "OK",
      "schedule" -> Json.toJson(ScheduleController.toSchedule(gen.elem)),
      "number of conflicts" -> gen.evaluate.err.size // TODO serialize conflicts
    ))))
  }

  def previewAtomic(course: String, labwork: String) = previewWith(course, labwork) { gen =>
    import utils.Ops._
    import MonadInstances.{optM, tryM}

    gen.map(ScheduleController.toSchedule).map(atomize).elem.peek(json =>
      Ok(Json.obj(
        "status" -> "OK",
        "schedule" -> json,
        "number of conflicts" -> gen.evaluate.err.size // TODO serialize conflicts
      ))
    )
  }

  def header = Action { implicit request =>
    NoContent.as(mimeType)
  }

  private def createWith(course: String)(toResult: Schedule => Try[Result]) = restrictedContext(course)(Create) contentTypedAction { implicit request =>
    request.body.validate[Schedule].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success =>
        repository.add[Schedule](success).flatMap(_ => toResult(success)) match {
          case Success(result) =>
            result
          case Failure(e) =>
            InternalServerError(Json.obj(
              "status" -> "KO",
              "errors" -> e.getMessage
            ))
        }
    )
  }

  private def previewWith(course: String, labwork: String)(toResult: Gen[ScheduleG, Conflict, Int] => Try[Option[Result]]) = restrictedContext(course)(Create) action { implicit request =>
    import ScheduleController._
    import utils.Ops._
    import MonadInstances.{optM, tryM}
    import TraverseInstances.{travO, travT}

    implicit val gb = defaultBindings.GroupBinding.groupBinder
    implicit val gcu = defaultBindings.GroupBinding.classUri
    implicit val lb = defaultBindings.LabworkBinding.labworkBinder
    implicit val sb = defaultBindings.SemesterBinding.semesterBinder
    implicit val tb = defaultBindings.TimetableBinding.timetableBinder
    implicit val tcu = defaultBindings.TimetableBinding.classUri
    implicit val ab = defaultBindings.AssignmentPlanBinding.assignmentPlanBinder
    implicit val abu = defaultBindings.AssignmentPlanBinding.classUri

    val id = UUID.fromString(labwork)

    val genesis = for {
      lab <- repository.get[Labwork](Labwork.generateUri(UUID.fromString(labwork)))
      semester <- lab.map(l => repository.get[Semester](Semester.generateUri(l.semester))).sequenceM
      groups <- repository.get[Group].map(_.filter(_.labwork == id))
      timetable <- repository.get[Timetable].map(_.find(_.labwork == id))
      plans <- repository.get[AssignmentPlan].map(_.find(_.labwork == id))
      comp <- competitive(id, repository)
    } yield for {
      t <- timetable if t.entries.nonEmpty
      p <- plans if p.entries.nonEmpty
      s <- semester.flatten
      g <- if (groups.nonEmpty) Some(groups) else None
    } yield scheduleGenesisService.generate(t, g, p, s, comp.toVector)._1

    genesis flatPeek toResult match {
      case Success(s) =>
        s match {
          case Some(result) => result
          case None =>
            NotFound(Json.obj(
              "status" -> "KO",
              "message" -> "No such element..."
            ))
        }
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, schedule.create)
    case Delete => SecureBlock(restrictionId, schedule.delete)
    case _ => PartialSecureBlock(god)
  }

  override protected def atomize(output: Schedule): Try[Option[JsValue]] = {
    import defaultBindings.EmployeeBinding.employeeBinder
    import defaultBindings.GroupBinding.groupBinder
    import defaultBindings.LabworkBinding._
    import defaultBindings.RoomBinding.roomBinder

    for {
      labwork <- repository.get[Labwork](Labwork.generateUri(output.labwork)(namespace))
      rooms <- repository.getMany[Room](output.entries.map(e => Room.generateUri(e.room)(namespace)))
      supervisors <- repository.getMany[Employee](output.entries.map(e => User.generateUri(e.supervisor)(namespace)))
      groups <- repository.getMany[Group](output.entries.map(e => Group.generateUri(e.group)(namespace)))
    } yield labwork.map { l =>
      val entries = output.entries.foldLeft(Set.empty[ScheduleEntryAtom]) { (newSet, e) =>
        (for {
          r <- rooms.find(_.id == e.room)
          s <- supervisors.find(_.id == e.supervisor)
          g <- groups.find(_.id == e.group)
        } yield ScheduleEntryAtom(l, e.start, e.end, e.date, r, s, g, e.id)) match {
          case Some(atom) => newSet + atom
          case None => newSet
        }
      }
      Json.toJson(ScheduleAtom(l, entries, output.published, output.id))(Schedule.atomicWrites)
    }
  }
}