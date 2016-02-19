package controllers.crud.schedule

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models._
import models.schedule.{ScheduleEntry, Timetable, Schedule, ScheduleProtocol}
import models.security.Permissions._
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.Result
import services._
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Success, Failure, Try}

object ScheduleCRUDController {

  def competitive(labwork: UUID, repository: SesameRepository): Try[Vector[ScheduleG]] = {
    scheduleFor(labwork, repository).map {
      case Some(s) => s.flatMap(ss => toScheduleG(ss, repository))
      case None => Vector.empty[ScheduleG]
    }
  }

  private def scheduleFor(labwork: UUID, repository: SesameRepository): Try[Option[Vector[Schedule]]] = {
    lazy val lwm = LWMPrefix[repository.Rdf]
    val bindings = Bindings[repository.Rdf](repository.namespace)

    import store.sparql.select._
    import store.sparql.select
    import bindings.ScheduleBinding._
    import bindings.ScheduleEntryBinding._

    import utils.Ops._
    import TraverseInstances._
    import MonadInstances.{tryM, listM}

    lazy val id = Labwork.generateUri(labwork)(repository.namespace)

    val query = select distinct "schedules" where {
      ^(s(id), p(lwm.course), v("courseid")) .
        ^(s(id), p(lwm.semester), v("semester")) .
        ^(v("course"), p(lwm.id), v("courseid")) .
        ^(v("course"), p(lwm.semesterIndex), v("semesterIndex")) .
        ^(v("labwork"), p(lwm.semester), v("semester")) .
        ^(v("labwork"), p(lwm.course), v("course2id")) .
        ^(v("course2"), p(lwm.id), v("course2id")) .
        ^(v("course2"), p(lwm.semesterIndex), v("semesterIndex")) .
        ^(v("labwork"), p(lwm.id), v("labworkid")) .
        ^(v("schedules"), p(lwm.labwork), v("labworkid"))
    }

    repository.query(query)
      .flatMap(_.get("schedules").peak(_.stringValue()))
      .map(repository.getMany[Schedule])
      .sequenceM
  }

  private def toScheduleG(schedule: Schedule, repository: SesameRepository): Option[ScheduleG] = {
    val bindings = Bindings[repository.Rdf](repository.namespace)
    import bindings.GroupBinding.groupBinder
    import bindings.GroupBinding._

    import utils.Ops._
    import MonadInstances._

    val maybeEntries = sequence(
      schedule.entries flatMap { entry =>
        val group = repository.get[Group](Group.generateUri(entry.group)(repository.namespace)).toOption
        group.peak(g => ScheduleEntryG(entry.start, entry.end, entry.date, entry.room, entry.supervisor, g, entry.id))
      })

    maybeEntries map (entries => ScheduleG(schedule.labwork, entries, schedule.id))
  }
}

class ScheduleCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService, val scheduleGenesisService: ScheduleGenesisServiceLike) extends AbstractCRUDController[ScheduleProtocol, Schedule] {

  override implicit def rdfWrites: ToPG[Sesame, Schedule] = defaultBindings.ScheduleBinding.scheduleBinder

  override implicit def rdfReads: FromPG[Sesame, Schedule] = defaultBindings.ScheduleBinding.scheduleBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Schedule] = defaultBindings.ScheduleBinding.classUri

  override implicit def uriGenerator: UriGenerator[Schedule] = Schedule

  override protected def fromInput(input: ScheduleProtocol, id: Option[UUID]): Schedule = id match {
    case Some(uuid) => Schedule(input.labwork, input.entries, uuid)
    case None => Schedule(input.labwork, input.entries, Schedule.randomUUID)
  }

  override def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Schedule]): Result = ???

  override implicit def reads: Reads[ScheduleProtocol] = Schedule.reads

  override implicit def writes: Writes[Schedule] = Schedule.writes

  override implicit val mimeType: LwmMimeType = LwmMimeType.scheduleV1Json

  // /labworks/:labwork/schedules/preview
  def preview(labwork: String) = restrictedContext(labwork)(Create) action { implicit request =>
    import utils.Ops._
    import MonadInstances.{tryM, optM}

    implicit val gb = defaultBindings.GroupBinding.groupBinder
    implicit val gcu = defaultBindings.GroupBinding.classUri
    implicit val tb = defaultBindings.TimetableBinding.timetableBinder
    implicit val tcu = defaultBindings.TimetableBinding.classUri
    implicit val lb = defaultBindings.LabworkBinding.labworkBinder

    val id = UUID.fromString(labwork)
    val uri = Labwork.generateUri(id)(namespace)

    val gen = for {
      groups <- repository.get[Group].map(_.filter(_.labwork == id))
      timetable <- repository.get[Timetable].map(_.find(_.labwork == id))
      plan <- repository.get[Labwork](uri).peak(_.assignmentPlan)
      comp <- ScheduleCRUDController.competitive(id, repository)
    } yield {
      for {
        t <- timetable if t.entries.nonEmpty
        p <- plan if p.entries.nonEmpty
        g <- if (groups.nonEmpty) Some(groups) else None
      } yield scheduleGenesisService.generate(t, g, p, comp)._1
    }

    gen match {
      case Success(s) =>
        s match {
          case Some(ss) =>
            val schedule = {
              val entries = ss.elem.entries.map(e => ScheduleEntry(e.start, e.end, e.date, e.room, e.supervisor, e.group.id, e.id))
              Schedule(ss.elem.labwork, entries, ss.elem.id)
            }

            Ok(Json.obj(
              "status" -> "OK",
              "schedule" -> Json.toJson(schedule),
              "number of conflicts" -> ss.evaluate.err.size // TODO serialize conflicts
            ))
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

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Set(prime)) // TODO to adjust
  }

  override protected def restrictedContext(moduleId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(moduleId, Set(createSchedule)) // TODO to adjust
    case _ => PartialSecureBlock(Set(prime))
  }
}