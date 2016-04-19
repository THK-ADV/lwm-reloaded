package controllers.schedule

import java.util.UUID

import controllers.crud._
import controllers.schedule.ScheduleEntryController._
import models.labwork._
import models.security.Permissions.{god, scheduleEntry}
import models.users.{Employee, User}
import models.{Course, Room, UriGenerator}
import modules.store.BaseNamespace
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import play.api.mvc._
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.sparql.select
import store.sparql.select._
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import utils.Ops.MonadInstances.{optM, tryM}
import utils.Ops.TraverseInstances.{travO, travT}
import utils.Ops._
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
    with BaseNamespace
    with Filterable[ScheduleEntry]
    with ContentTyped
    with Secured
    with SessionChecking
    with SecureControllerContext
    with Chunkable[ScheduleEntry]
    with Atomic[ScheduleEntry]
    with SesameRdfSerialisation[ScheduleEntry]
    with JsonSerialisation[ScheduleEntry, ScheduleEntry] {

  override implicit def reads: Reads[ScheduleEntry] = ScheduleEntry.reads

  override implicit def writes: Writes[ScheduleEntry] = ScheduleEntry.writes

  override implicit def rdfWrites: ToPG[Sesame, ScheduleEntry] = defaultBindings.ScheduleEntryBinding.scheduleEntryBinder

  override implicit def rdfReads: FromPG[Sesame, ScheduleEntry] = defaultBindings.ScheduleEntryBinding.scheduleEntryBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, ScheduleEntry] = defaultBindings.ScheduleEntryBinding.classUri

  override implicit def uriGenerator: UriGenerator[ScheduleEntry] = ScheduleEntry

  override implicit val mimeType: LwmMimeType = LwmMimeType.scheduleEntryV1Json

  def allFrom(course: String) = restrictedContext(course)(GetAll) action { implicit request =>
    chunkedAll(chunkSimple)(rebase(ScheduleEntry.generateBase, courseAttribute -> Seq(course)))
  }

  def allAtomicFrom(course: String) = restrictedContext(course)(GetAll) action { implicit request =>
    chunkedAll(chunkAtoms)(rebase(ScheduleEntry.generateBase, courseAttribute -> Seq(course)))
  }

  def allFromLabwork(course: String, labwork: String) = restrictedContext(course)(GetAll) action { implicit request =>
    chunkedAll(chunkSimple)(rebase(ScheduleEntry.generateBase, courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork)))
  }

  def allAtomicFromLabwork(course: String, labwork: String) = restrictedContext(course)(GetAll) action { implicit request =>
    chunkedAll(chunkAtoms)(rebase(ScheduleEntry.generateBase, courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork)))
  }

  def get(course: String, entry: String) = restrictedContext(course)(Get) action { request =>
    val result = repository.get[ScheduleEntry]((UUID.fromString _ andThen ScheduleEntry.generateUri)(entry))
    handle(result)(entry => Ok(Json.toJson(entry)).as(mimeType))
  }

  def getAtomic(course: String, entry: String) = restrictedContext(course)(Get) action { request =>
    val result = repository.get[ScheduleEntry]((UUID.fromString _ andThen ScheduleEntry.generateUri)(entry)).flatPeek(atomize)
    handle(result)(json => Ok(json).as(mimeType))
  }

  def update(course: String, entry: String) = updateEntry(course, entry) { entry =>
    Success(Ok(Json.toJson(entry)).as(mimeType))
  }

  def updateAtomic(course: String, entry: String) = updateEntry(course, entry) { entry =>
    Success(handle(atomize(entry))(json => Ok(json).as(mimeType)))
  }

  def header = Action { implicit request =>
    NoContent.as(mimeType)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[ScheduleEntry]): Try[Set[ScheduleEntry]] = {
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
        if (split.size != 2)
          Failure(new Throwable("A range can only have two parameters"))
        else
          clause map {
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
    import ScheduleEntry.format
    import defaultBindings.EmployeeBinding.employeeBinder
    import defaultBindings.GroupBinding.groupBinder
    import defaultBindings.LabworkBinding.labworkBinder
    import defaultBindings.RoomBinding.roomBinder

    for {
      mbLabwork <- repository.get[Labwork](Labwork.generateUri(output.labwork))
      mbGroup <- repository.get[Group](Group.generateUri(output.group))
      mbSupervisor <- repository.get[Employee](User.generateUri(output.supervisor))
      mbRoom <- repository.get[Room](Room.generateUri(output.room))
    } yield for {
      labwork <- mbLabwork; group <- mbGroup; supervisor <- mbSupervisor; room <- mbRoom
    } yield Json.toJson(
      ScheduleEntryAtom(labwork, output.start, output.end, output.date, room, supervisor, group, output.id)
    )
  }

  def chunkedAll(chunks: Set[ScheduleEntry] => Enumerator[JsValue])(implicit request: Request[AnyContent]) = {
    (if (request.queryString.isEmpty)
      repository.get[ScheduleEntry] map chunks
    else
      getWithFilter(request.queryString)(Set.empty) map chunks) match {
        case Success(enum) =>
          Ok.chunked(enum).as(mimeType)
        case Failure(e) =>
          InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> e.getMessage
          ))
    }
  }

  private def handle[A](attempt: Try[Option[A]])(toResult: A => Result): Result = attempt match {
    case Success(Some(element)) =>
      toResult(element)
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

  private def updateEntry(course: String, entry: String)(toResult: ScheduleEntry => Try[Result]) = restrictedContext(course)(Update) contentTypedAction { request =>
    request.body.validate[ScheduleEntry].fold(
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

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case GetAll => SecureBlock(restrictionId, scheduleEntry.getAll)
    case Get => SecureBlock(restrictionId, scheduleEntry.get)
    case Update => SecureBlock(restrictionId, scheduleEntry.update)
    case _ => PartialSecureBlock(god)
  }
}
