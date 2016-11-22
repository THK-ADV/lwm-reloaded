package controllers

import java.util.UUID

import models.Permissions.{god, reportCardEntry}
import models._
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.{Controller, Request}
import services.{ReportCardServiceLike, RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.sparql.{Clause, NoneClause, SelectClause}
import store.{Namespace, SesameRepository}
import utils.{Attempt, Continue, LwmMimeType, Return}
import controllers.ReportCardEntryController._
import modules.BaseNamespace

import scala.collection.Map
import scala.util.{Failure, Success, Try}

object ReportCardEntryController {
  val studentAttribute = "student"
  val courseAttribute = "course"
  val labworkAttribute = "labwork"
  val roomAttribute = "room"
  val dateAttribute = "date"
  val startAttribute = "start"
  val endAttribute = "end"
}

// TODO inherit from AbstractCRUDController
class ReportCardEntryController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService, val reportCardService: ReportCardServiceLike)
  extends Controller
    with BaseNamespace
    with JsonSerialisation[ReportCardEntry, ReportCardEntry, ReportCardEntryAtom]
    with RdfSerialisation[ReportCardEntry, ReportCardEntryAtom]
    with ContentTyped
    with Chunked
    with Secured
    with SessionChecking
    with SecureControllerContext
    with Stored
    with ModelConverter[ReportCardEntry, ReportCardEntry]
    with Consistent[ReportCardEntry, ReportCardEntry]
    with Filterable[ReportCardEntry]
    with Basic[ReportCardEntry, ReportCardEntry, ReportCardEntryAtom]
    with RequestRebase[ReportCardEntry] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardEntryV1Json

  override implicit val descriptor: Descriptor[Sesame, ReportCardEntry] = defaultBindings.ReportCardEntryDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, ReportCardEntryAtom] = defaultBindings.ReportCardEntryAtomDescriptor

  override implicit val reads: Reads[ReportCardEntry] = ReportCardEntry.reads

  override implicit val writes: Writes[ReportCardEntry] = ReportCardEntry.writes

  override implicit val writesAtom: Writes[ReportCardEntryAtom] = ReportCardEntry.writesAtom

  override implicit val uriGenerator: UriGenerator[ReportCardEntry] = ReportCardEntry

  override protected def compareModel(input: ReportCardEntry, output: ReportCardEntry): Boolean = input == output

  override protected def fromInput(input: ReportCardEntry, existing: Option[ReportCardEntry]): ReportCardEntry = input

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(reportCardEntry.get)
    case _ => PartialSecureBlock(god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, reportCardEntry.create)
    case Update => SecureBlock(restrictionId, reportCardEntry.update)
    case GetAll => SecureBlock(restrictionId, reportCardEntry.getAll)
    case _ => PartialSecureBlock(god)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[ReportCardEntry]): Try[Set[ReportCardEntry]] = {
    import controllers.ReportCardEntryController._
    import store.sparql.select
    import store.sparql.select._
    import utils.Ops.MonadInstances.listM

    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]

    queryString.foldLeft(Try((**(v("entries"), p(rdf.`type`), s(lwm.ReportCardEntry)), NoneClause: Clause))) {
      case (clause, (`studentAttribute`, values)) => clause map {
        case ((filter, resched)) =>
          (filter append **(v("entries"), p(lwm.student), s(User.generateUri(UUID.fromString(values.head)))), resched)
      }
      case (clause, (`courseAttribute`, values)) => clause map {
        case ((filter, resched)) =>
          (filter append **(v("entries"), p(lwm.labwork), v("labwork")).
            **(v("labwork"), p(lwm.course), s(Course.generateUri(UUID.fromString(values.head)))), resched)
      }
      case (clause, (`labworkAttribute`, values)) => clause map {
        case ((filter, resched)) =>
          (filter append **(v("entries"), p(lwm.labwork), s(Labwork.generateUri(UUID.fromString(values.head)))), resched)
      }
      case (clause, (`roomAttribute`, values)) => clause map {
        case ((filter, resched)) =>
          (filter append **(v("entries"), p(lwm.room), s(Room.generateUri(UUID.fromString(values.head)))),
            resched.**(v("rescheduled"), p(lwm.room), s(Room.generateUri(UUID.fromString(values.head)))))
      }
      case (clause, (`dateAttribute`, values)) => clause map {
        case ((filter, resched)) =>
          (filter append **(v("entries"), p(lwm.date), v("date")).filterStrStarts(v("date"), values.head),
            resched.filterStrStarts(v("rdate"), values.head))
      }
      case (clause, (`startAttribute`, values)) => clause map {
        case ((filter, resched)) =>
          (filter append **(v("entries"), p(lwm.start), v("start")).filterStrStarts(v("start"), values.head),
            resched.filterStrStarts(v("rstart"), values.head))
      }
      case (clause, (`endAttribute`, values)) => clause map {
        case ((filter, resched)) =>
          (filter append **(v("entries"), p(lwm.end), v("end")).filterStrStarts(v("end"), values.head),
            resched.filterStrStarts(v("rend"), values.head))
      }
      case _ => Failure(new Throwable("Unknown attribute"))
    } flatMap {
      case ((clause, resched)) =>
        val query = select distinct "entries" where {
          clause.optional {
            **(v("entries"), p(lwm.rescheduled), v("rescheduled")).
              **(v("rescheduled"), p(lwm.date), v("rdate")).
              **(v("rescheduled"), p(lwm.start), v("rstart")).
              **(v("rescheduled"), p(lwm.end), v("rend")) append
              resched
          }
        }

        repository.prepareQuery(query).
          select(_.get("entries")).
          transform(_.fold(List.empty[Value])(identity)).
          map(_.stringValue).
          requestAll(repository.getMany[ReportCardEntry](_)).
          run
    }
  }

  def get(student: String) = contextFrom(Get) action { implicit request =>
    val rebased = rebase(studentAttribute -> Seq(student))

    filter(rebased)(Set.empty)
      .mapResult(entries => Ok(Json.toJson(entries)).as(mimeType))
  }

  def getAtomic(student: String) = contextFrom(Get) action { implicit request =>
    val rebased = rebase(studentAttribute -> Seq(student))

    filter(rebased)(Set.empty)
      .flatMap(set => retrieveLots[ReportCardEntryAtom](set map ReportCardEntry.generateUri))
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def all(course: String) = restrictedContext(course)(GetAll) action { implicit request =>
    val rebased = rebase(courseAttribute -> Seq(course))

    filter(rebased)(Set.empty)
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def allAtomic(course: String) = restrictedContext(course)(GetAll) action { implicit request =>
    val rebased = rebase(courseAttribute -> Seq(course))

    filter(rebased)(Set.empty)
      .flatMap(set => retrieveLots[ReportCardEntryAtom](set map ReportCardEntry.generateUri))
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def update(course: String, entry: String) = restrictedContext(course)(Update) contentTypedAction { request =>
    updateEntry(request, entry)
      .mapResult(entry => Ok(Json.toJson(entry)).as(mimeType))
  }

  def updateAtomic(course: String, entry: String) = restrictedContext(course)(Update) contentTypedAction { request =>
    updateEntry(request, entry)
      .flatMap(entry => retrieve[ReportCardEntryAtom](ReportCardEntry.generateUri(entry)))
      .mapResult(entry => Ok(Json.toJson(entry)).as(mimeType))
  }

  def allFromScheduleEntry(course: String, scheduleEntry: String) = restrictedContext(course)(GetAll) action { request =>
    fromScheduleEntry(scheduleEntry)
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def allAtomicFromScheduleEntry(course: String, scheduleEntry: String) = restrictedContext(course)(GetAll) action { request =>
    fromScheduleEntry(scheduleEntry)
      .flatMap(set => retrieveLots[ReportCardEntryAtom](set map ReportCardEntry.generateUri))
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def create(course: String, schedule: String) = restrictedContext(course)(Create) contentTypedAction { request =>
    import controllers.ScheduleController._
    import defaultBindings.{ScheduleDescriptor, AssignmentPlanDescriptor}
    import store.sparql.select
    import store.sparql.select._
    import utils.Ops.MonadInstances.optM
    import utils.Ops.NaturalTrasformations._
    import utils.Ops.TraverseInstances.travO

    import scalaz.syntax.applicative._

    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]
    val scheduleId = UUID.fromString(schedule)
    val scheduleUri = Schedule.generateUri(scheduleId)

    val query = select("plan") where {
      **(v("plan"), p(rdf.`type`), s(lwm.AssignmentPlan)).
        **(s(scheduleUri), p(lwm.labwork), v("labwork")).
        **(v("plan"), p(lwm.labwork), v("labwork"))
    }

    val attemptPlan = repository.prepareQuery(query).
      select(_.get("plan")).
      changeTo(_.headOption).
      map(_.stringValue())(optM).
      request(repository.get[AssignmentPlan](_))

    (for {
      optPlan <- attemptPlan.run
      optSchedule <- repository.get[Schedule](scheduleUri)
      optScheduleG = optSchedule flatMap (toScheduleG(_, repository))
      reportCards = (optScheduleG |@| optPlan) (reportCardService.reportCards) getOrElse Set.empty[ReportCardEntry]
      _ <- repository addMany reportCards
    } yield reportCards) match {
      case Success(_) =>
        Created(Json.obj(
          "status" -> "OK",
          "message" -> s"Created report card entries for schedule $schedule"
        ))
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  def updateEntry(request: Request[JsValue], entry: String): Attempt[ReportCardEntry] = {
    validate(request)
      .when(_.id == UUID.fromString(entry), overwrite0)(
        BadRequest(Json.obj(
          "status" -> "KO",
          "message" -> s"Id found in body  does not match id found in resource ($entry)"
        )))
  }

  def fromScheduleEntry(entry: String): Attempt[Set[ReportCardEntry]] = {
    import store.sparql.select
    import store.sparql.select._
    import utils.Ops.MonadInstances._

    import scalaz.syntax.applicative._

    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]

    def getEntries(query: SelectClause) = {
      repository.prepareQuery(query).
        select(_.get("entries")).
        transform(_.fold(List.empty[Value])(identity)).
        map(_.stringValue()).
        requestAll(repository.getMany[ReportCardEntry](_)).
        run
    }

    val scheduleEntryUri = ScheduleEntry.generateUri(UUID.fromString(entry))(namespace)

    val entryQuery = select distinct("entries", "rescheduled") where {
      **(s(scheduleEntryUri), p(lwm.labwork), v("labwork")).
        **(s(scheduleEntryUri), p(lwm.room), v("room")).
        **(s(scheduleEntryUri), p(lwm.date), v("date")).
        **(s(scheduleEntryUri), p(lwm.start), v("start")).
        **(s(scheduleEntryUri), p(lwm.end), v("end")).
        **(v("entries"), p(rdf.`type`), s(lwm.ReportCardEntry)).
        **(v("entries"), p(lwm.room), v("room")).
        **(v("entries"), p(lwm.date), v("date")).
        **(v("entries"), p(lwm.start), v("start")).
        **(v("entries"), p(lwm.end), v("end"))
    }

    val rescheduledQuery = select distinct("entries", "rescheduled") where {
      **(s(scheduleEntryUri), p(lwm.labwork), v("labwork")).
        **(s(scheduleEntryUri), p(lwm.room), v("room")).
        **(s(scheduleEntryUri), p(lwm.date), v("date")).
        **(s(scheduleEntryUri), p(lwm.start), v("start")).
        **(s(scheduleEntryUri), p(lwm.end), v("end")).
        **(v("entries"), p(rdf.`type`), s(lwm.ReportCardEntry)).
        **(v("entries"), p(lwm.rescheduled), v("rescheduled")).
        **(v("rescheduled"), p(lwm.date), v("date")).
        **(v("rescheduled"), p(lwm.start), v("start")).
        **(v("rescheduled"), p(lwm.end), v("end")).
        **(v("rescheduled"), p(lwm.room), v("room"))
    }

    (getEntries(entryQuery) |@| getEntries(rescheduledQuery)) (_ ++ _) match {
      case Success(entries) =>
        Continue(entries)
      case Failure(e) => Return(
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }
}