package controllers.crud.labwork

import java.util.UUID

import controllers.crud._
import models.{Course, Room}
import models.labwork._
import models.users.{Employee, User}
import modules.store.BaseNamespace
import org.w3.banana.RDFPrefix
import play.api.libs.json.{JsError, JsValue, Json}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import store.sparql.select
import store.sparql.select._
import utils.Ops._
import ScheduleEntryController._
import play.api.libs.iteratee.Enumerator
import play.api.mvc.{AnyContent, Controller, Request, Result}
import store.bind.Bindings
import utils.Ops.MonadInstances.{optM, tryM}
import utils.Ops.TraverseInstances.{travO, travT}
import utils.RequestOps._

import scala.collection.Map
import scala.util.{Failure, Success, Try}

object ScheduleEntryController {
  val courseAttribute = "course"
  val labworkAttribute = "labwork"
  val groupAttribute = "group"
  val supervisorAttribute = "supervisor"
  val dateAttribute = "date"
  val startAttribute = "start"
  val endAttribute = "end"
  val dateRangeAttribute = "dateRange"
}

class ScheduleEntryController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService)
  extends Controller
    with Filterable[ScheduleEntry]
    with BaseNamespace
    with ContentTyped
    with Secured
    with SessionChecking
    with SecureControllerContext
    with Chunkable[ScheduleEntry]
    with Atomic[ScheduleEntry] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.scheduleEntryV1Json
  val bindings = Bindings[repository.Rdf](namespace)


  def allFrom(course: String) = restrictedContext(course)(GetAll) action { request =>
    implicit val req = rebase[AnyContent](request.uri, courseAttribute -> Seq(course))(request)
    chunkedAll(chunkSimple)
  }

  def allAtomicFrom(course: String) = restrictedContext(course)(GetAll) action { request =>
    implicit val req = rebase[AnyContent](request.uri, courseAttribute -> Seq(course))(request)
    chunkedAll(chunkAtoms)
  }

  def allFromLabwork(course: String, labwork: String) = restrictedContext(course)(GetAll) action { request =>
    implicit val req = rebase[AnyContent](request.uri, courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork))(request)
    chunkedAll(chunkSimple)
  }

  def allAtomicFromLabwork(course: String, labwork: String) = restrictedContext(course)(GetAll) action { request =>
    implicit val req = rebase[AnyContent](request.uri, courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork))(request)
    chunkedAll(chunkAtoms)
  }

  def get(course: String, entry: String) = restrictedContext(course)(Get) action { request =>
    import bindings.ScheduleEntryBinding._

    val action =
      UUID.fromString _ andThen
      ScheduleEntry.generateUri andThen
      (id => repository.get[ScheduleEntry](id)) andThen
      (attempt => handle(attempt)(r => Ok(Json.toJson(r)).as(mimeType)))

    action(entry)
  }

  def getAtomic(course: String, entry: String) = restrictedContext(course)(Get) action { request =>
    import bindings.ScheduleEntryBinding._

    val action =
      UUID.fromString _ andThen
      ScheduleEntry.generateUri andThen
      (id => repository.get[ScheduleEntry](id)) andThen
      (_ flatPeek atomize) andThen
      (attempt => handle(attempt)(r => Ok(Json.toJson(r)).as(mimeType)))

    action(entry)
  }

  def update(course: String, entry: String) = updateEntry(course, entry)(sentry => Success(Ok(Json.toJson(sentry)).as(mimeType)))
  def updateAtomic(course: String, entry: String) = updateEntry(course, entry) { sentry =>
    val action = atomize _ andThen (attempt => handle(attempt)(js => Ok(js).as(mimeType)))
    Success(action(sentry))
  }

  //FOT TEST PURPOSES. DO NOT DELETE
  def all(course: String) = restrictedContext(course)(GetAll) action { implicit request =>
    chunkedAll(chunkSimple)
  }
  def allAtomic(course: String) = restrictedContext(course)(GetAll) action { implicit request =>
    chunkedAll(chunkAtoms)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[ScheduleEntry]): Try[Set[ScheduleEntry]] = {
    import bindings.ScheduleEntryBinding._

    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]

    val startClause = ^(v("entries"), p(rdf.`type`), s(lwm.ScheduleEntry))

    queryString.foldRight(Try(startClause)) {
      case ((`courseAttribute`, values), clause) => clause map {
        _ append ^(v("entries"), p(lwm.labwork), v("labwork")) . ^(v("labwork"), p(lwm.course), s(Course.generateUri(UUID.fromString(values.head))))
      }
      case ((`labworkAttribute`, values), clause) => clause map {
        _ append ^(v("entries"), p(lwm.labwork), s(Labwork.generateUri(UUID.fromString(values.head))))
      }
      case ((`groupAttribute`, values), clause) => clause map {
        _ append ^(v("entries"), p(lwm.group), s(Group.generateUri(UUID.fromString(values.head))))
      }
      case ((`supervisorAttribute`, values), clause) => clause map {
        _ append ^(v("entries"), p(lwm.supervisor), s(User.generateUri(UUID.fromString(values.head))))
      }
      case ((`startAttribute`, values), clause) => clause map {
      _ append ^(v("entries"), p(lwm.start), v("start")) . filterStrStarts(v("start"), values.head)
      }
      case ((`endAttribute`, values), clause) => clause map {
        _ append ^(v("entries"), p(lwm.end), v("end")) . filterStrStarts(v("end"), values.head)
      }
      case ((`dateAttribute`, values), clause) => clause map {
        _ append ^(v("entries"), p(lwm.date), v("date")) . filterStrStarts(v("date"), values.head)
      }
      case ((`dateRangeAttribute`, values), clause) =>
        val split = values.head.split(",").toVector
        if(split.size != 2) Failure(new Throwable("A range can only have two parameters"))
        else clause map {
          _ append ^(v("entries"), p(lwm.date), v("date")) . filter(s"str(?date) >= '${split(0)}' && str(?date) <= '${split(1)}'")
        }

      case ((_, _), clause) => Failure(new Throwable("Unknown attribute"))
    } flatMap { clause =>
      val query = select distinct "entries" where clause

      repository.prepareQuery(query).
        select(_.get("entries")).
        transform(_.fold(List.empty[String])(_ map (_.stringValue()))).
        requestAll(repository.getMany[ScheduleEntry]).
        run
    }
  }

  override protected def atomize(output: ScheduleEntry): Try[Option[JsValue]] = {
    import bindings.GroupBinding._
    import bindings.EmployeeBinding._
    import bindings.RoomBinding._
    import ScheduleEntry.format
    for {
      mgroup <- repository.get[Group](Group.generateUri(output.group))
      msupervisor <- repository.get[Employee](User.generateUri(output.supervisor))
      mroom <- repository.get[Room](Room.generateUri(output.room))
    } yield for {
      group <- mgroup
      supervisor <- msupervisor
      room <- mroom
    } yield Json.toJson(ScheduleEntryAtom(output.start, output.end, output.date, room, supervisor, group))
  }

  private def chunkedAll(f: Set[ScheduleEntry] => Enumerator[JsValue])(implicit request: Request[AnyContent]) = {
    import bindings.ScheduleEntryBinding._

    (if(request.queryString.isEmpty)
      repository.get[ScheduleEntry] map f
    else
      getWithFilter(request.queryString)(Set.empty) map f) match {
      case Success(enum) => Ok.chunked(enum).as(mimeType)
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  private def handle[A](attempt: Try[Option[A]])(f: A => Result): Result = attempt match {
    case Success(Some(element)) => f(element)
    case Success(None) =>
      NotFound(Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      ))
    case Failure(e) =>
      InternalServerError(Json.obj(
        "status" -> "KO",
        "errors" -> e.getMessage
      ))
  }

  private def updateEntry(course: String, entry: String)(f: ScheduleEntry => Try[Result]) = restrictedContext(course)(Update) contentTypedAction { request =>
    import bindings.ScheduleEntryBinding._
    request.body.validate[ScheduleEntry].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => {
        if (success.id == UUID.fromString(entry))
          repository.update(success)(scheduleEntryBinder, ScheduleEntry).flatMap(_ => f(success)) match {
            case Success(result) => result
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

  //TODO: Add necessary permissions
  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = super.restrictedContext(restrictionId)
}
