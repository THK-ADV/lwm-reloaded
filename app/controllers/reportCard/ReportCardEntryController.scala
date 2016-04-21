package controllers.reportCard

import java.util.UUID

import controllers.crud._
import controllers.schedule.ScheduleController
import models.{Room, UriGenerator}
import models.labwork._
import models.users.{Student, User}
import modules.store.BaseNamespace
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.{Controller, Result}
import services.{ReportCardServiceLike, RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import models.security.Permissions.{god, reportCardEntry}
import store.sparql.{Clause, NoneClause}

import scala.util.{Failure, Success, Try}

object ReportCardEntryController {
  val studentAttribute = "student"
  val labworkAttribute = "labwork"
  val roomAttribute = "room"
  val dateAttribute = "date"
  val startAttribute = "start"
  val endAttribute = "end"
}

class ReportCardEntryController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService, val reportCardService: ReportCardServiceLike)
  extends Controller
    with BaseNamespace
    with JsonSerialisation[ReportCardEntry, ReportCardEntry]
    with SesameRdfSerialisation[ReportCardEntry]
    with ContentTyped
    with Secured
    with SessionChecking
    with SecureControllerContext
    with Atomic[ReportCardEntry] {

  override implicit def reads: Reads[ReportCardEntry] = ReportCardEntry.reads

  override implicit def writes: Writes[ReportCardEntry] = ReportCardEntry.writes

  override implicit def rdfReads: FromPG[Sesame, ReportCardEntry] = defaultBindings.ReportCardEntryBinding.reportCardEntryBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, ReportCardEntry] = defaultBindings.ReportCardEntryBinding.classUri

  override implicit def uriGenerator: UriGenerator[ReportCardEntry] = ReportCardEntry

  override implicit def rdfWrites: ToPG[Sesame, ReportCardEntry] = defaultBindings.ReportCardEntryBinding.reportCardEntryBinder

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardEntryV1Json

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

  def get(student: String) = forStudent(student) { entries =>
    Success(Ok(Json.toJson(entries)).as(mimeType))
  }

  def getAtomic(student: String) = forStudent(student) { entries =>
    atomizeMany(entries).map(json => Ok(json).as(mimeType))
  }

  def all(course: String) = reportCardEntries(course) { entries =>
    Success(Ok(Json.toJson(entries)).as(mimeType))
  }

  def allAtomic(course: String) = reportCardEntries(course) { entries =>
    atomizeMany(entries).map(json => Ok(Json.toJson(json)).as(mimeType))
  }

  def update(course: String, entry: String) = updateEntry(course, entry) { entry =>
    Success(Ok(Json.toJson(entry)).as(mimeType))
  }

  def updateAtomic(course: String, entry: String) = updateEntry(course, entry) { entry =>
    atomizeMany(Set(entry)).map(json => Ok(Json.toJson(json)).as(mimeType))
  }

  def allWithSchedule(course: String, scheduleEntry: String) = ???

  def create(course: String, schedule: String) = restrictedContext(course)(Create) contentTypedAction { request =>
    import scalaz.syntax.applicative._
    import store.sparql.select
    import store.sparql.select._
    import ScheduleController._
    import utils.Ops.MonadInstances.optM
    import utils.Ops.TraverseInstances.travO
    import utils.Ops.NaturalTrasformations._
    import defaultBindings.ScheduleBinding.scheduleBinder
    import defaultBindings.AssignmentPlanBinding.assignmentPlanBinder

    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]
    val scheduleId = UUID.fromString(schedule)
    val scheduleUri = Schedule.generateUri(scheduleId)

    val query = select("plan") where {
        **(v("plan"), p(rdf.`type`), s(lwm.AssignmentPlan)) .
        **(s(scheduleUri), p(lwm.labwork), v("labwork")) .
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
      reportCards = (optScheduleG |@| optPlan)(reportCardService.reportCards) getOrElse Set.empty[ReportCardEntry]
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

  private def updateEntry(course: String, entry: String)(toResult: ReportCardEntry => Try[Result]) = restrictedContext(course)(Update) contentTypedAction { request =>
    request.body.validate[ReportCardEntry].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => {
        if (success.id == UUID.fromString(entry))
          repository.update(success).flatMap(_ => toResult(success)) match {
            case Success(result) =>
              result
            case Failure(e) =>
              InternalServerError(Json.obj(
                "status" -> "KO",
                "errors" -> e.getMessage
              ))
          }
        else
          BadRequest(Json.obj(
            "status" -> "KO",
            "message" -> s"Id found in body (${success.id}) does not match id found in resource ($entry)"
          ))
      }
    )
  }

  private def reportCardEntries(course: String)(toResult: Set[ReportCardEntry] => Try[Result]) = restrictedContext(course)(GetAll) action { request =>
    import store.sparql.select._
    import store.sparql.select
    import ReportCardEntryController._
    import utils.Ops.MonadInstances.listM

    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]

    if (request.queryString.isEmpty)
      BadRequest(Json.obj(
        "status" -> "KO",
        "message" -> "Request should contain at least one attribute"
      ))
    else
      request.queryString.foldLeft(Try((**(v("entries"), p(rdf.`type`), s(lwm.ReportCardEntry)), NoneClause: Clause))) {
        case (clause, (`studentAttribute`, values)) => clause map {
          case ((filter, resched)) =>
            (filter append **(v("entries"), p(lwm.student), s(User.generateUri(UUID.fromString(values.head)))), resched)
        }
        case (clause, (`labworkAttribute`, values)) => clause map {
          case ((filter, resched)) =>
            (filter append **(v("entries"), p(lwm.labwork), s(Labwork.generateUri(UUID.fromString(values.head)))), resched)
        }
        case (clause, (`roomAttribute`, values)) => clause map {
          case ((filter, resched)) =>
            (filter append **(v("entries"), p(lwm.room), s(Room.generateUri(UUID.fromString(values.head)))),
              resched . **(v("rescheduled"), p(lwm.room), s(Room.generateUri(UUID.fromString(values.head)))))
        }
        case (clause, (`dateAttribute`, values)) => clause map {
          case ((filter, resched)) =>
            (filter append **(v("entries"), p(lwm.date), v("date")) . filterStrStarts(v("date"), values.head),
              resched . filterStrStarts(v("rdate"), values.head))
        }
        case (clause, (`startAttribute`, values)) => clause map {
          case ((filter, resched)) =>
            (filter append **(v("entries"), p(lwm.start), v("start")) . filterStrStarts(v("start"), values.head),
              resched . filterStrStarts(v("rstart"), values.head))
        }
        case (clause, (`endAttribute`, values)) => clause map {
          case ((filter, resched)) =>
            (filter append **(v("entries"), p(lwm.end), v("end")) . filterStrStarts(v("end"), values.head),
              resched . filterStrStarts(v("rend"), values.head))
        }
        case _ => Failure(new Throwable("Unknown attribute"))
      } flatMap {
        case ((clause, resched)) =>
          val query = select distinct "entries" where {
            clause . optional {
              **(v("entries"), p(lwm.rescheduled), v("rescheduled")) .
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
            run flatMap toResult
      } match {
        case Success(result) =>
          result
        case Failure(e) =>
          InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> e.getMessage
          ))
      }
  }

  private def forStudent(student: String)(toResult: Set[ReportCardEntry] => Try[Result]) = contextFrom(Get) action { request =>
    import store.sparql.select
    import store.sparql.select._
    import utils.Ops.MonadInstances.listM

    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]

    val query = select ("entries") where {
      **(v("entries"), p(rdf.`type`), s(lwm.ReportCardEntry)).
      **(v("entries"), p(lwm.student), s(User.generateUri(UUID.fromString(student))))
    }

    repository.prepareQuery(query).
      select(_.get("entries")).
      transform(_.fold(List.empty[Value])(identity)).
      map(_.stringValue()).
      requestAll(repository.getMany[ReportCardEntry](_)).
      run flatMap toResult match {
        case Success(result) =>
          result
        case Failure(e) =>
          InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> e.getMessage
          ))
      }
  }

  override protected def atomize(output: ReportCardEntry): Try[Option[JsValue]] = {
    import defaultBindings.StudentBinding.studentBinder
    import defaultBindings.LabworkBinding.labworkBinder
    import defaultBindings.RoomBinding.roomBinder
    import ReportCardEntry.atomicWrites

    for {
      optStudent <- repository.get[Student](User.generateUri(output.student))
      optLabwork <- repository.get[Labwork](Labwork.generateUri(output.labwork))
      optRoom <- repository.get[Room](Room.generateUri(output.room))
    } yield for {
      student <- optStudent; labwork <- optLabwork; room <- optRoom
    } yield Json.toJson(
      ReportCardEntryAtom(student, labwork, output.label, output.date, output.start, output.end, room, output.entryTypes, output.rescheduled, output.id)
    )
  }
}