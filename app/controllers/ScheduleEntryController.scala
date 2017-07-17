package controllers

import java.util.UUID

import models.Permissions.{god, scheduleEntry}
import models._
import modules.BaseNamespace
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc._
import services.{RoleService, RoleServiceLike, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.sparql.select
import store.sparql.select._
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import controllers.ScheduleEntryController._

import scala.util.{Failure, Try}

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

// TODO inherit from AbstractCRUDController
class ScheduleEntryController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleServiceLike)
  extends Controller
    with BaseNamespace
    with Filterable[SesameScheduleEntry]
    with ContentTyped
    with Secured
    with SessionChecking
    with SecureControllerContext
    with Chunked
    with Stored
    with RdfSerialisation[SesameScheduleEntry, SesameScheduleEntryAtom]
    with JsonSerialisation[SesameScheduleEntry, SesameScheduleEntry, SesameScheduleEntryAtom]
    with ModelConverter[SesameScheduleEntry, SesameScheduleEntry]
    with Consistent[SesameScheduleEntry, SesameScheduleEntry]
    with Basic[SesameScheduleEntry, SesameScheduleEntry, SesameScheduleEntryAtom]
    with RequestRebase[SesameScheduleEntry] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.scheduleEntryV1Json

  override implicit val descriptor: Descriptor[Sesame, SesameScheduleEntry] = defaultBindings.ScheduleEntryDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, SesameScheduleEntryAtom] = defaultBindings.ScheduleEntryAtomDescriptor

  override implicit val reads: Reads[SesameScheduleEntry] = SesameScheduleEntry.reads

  override implicit val writes: Writes[SesameScheduleEntry] = SesameScheduleEntry.writes

  override implicit val writesAtom: Writes[SesameScheduleEntryAtom] = SesameScheduleEntry.writesAtom

  override implicit val uriGenerator: UriGenerator[SesameScheduleEntry] = SesameScheduleEntry

  def coatomic(atom: SesameScheduleEntryAtom): SesameScheduleEntry = SesameScheduleEntry(atom.labwork.id, atom.start, atom.end, atom.date, atom.room.id, atom.supervisor map (_.id), atom.group.id, atom.invalidated, atom.id)

  override protected def compareModel(input: SesameScheduleEntry, output: SesameScheduleEntry): Boolean = input == output

  override protected def fromInput(input: SesameScheduleEntry, existing: Option[SesameScheduleEntry]): SesameScheduleEntry = input

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case GetAll => SecureBlock(restrictionId, scheduleEntry.getAll)
    case Get => SecureBlock(restrictionId, scheduleEntry.get)
    case Update => SecureBlock(restrictionId, scheduleEntry.update)
    case _ => PartialSecureBlock(god)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[SesameScheduleEntry]): Try[Set[SesameScheduleEntry]] = {
    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]

    val startClause = **(v("entries"), p(rdf.`type`), s(lwm.ScheduleEntry))
    // TODO appending queries are broken
    queryString.foldRight(Try(startClause)) {
      case ((`courseAttribute`, values), clause) => clause map {
        _ append **(v("entries"), p(lwm.labwork), v("labwork")).**(v("labwork"), p(lwm.course), s(SesameCourse.generateUri(UUID.fromString(values.head))))
      }
      case ((`labworkAttribute`, values), clause) => clause map {
        _ append **(v("entries"), p(lwm.labwork), s(SesameLabwork.generateUri(UUID.fromString(values.head))))
      }
      case ((`groupAttribute`, values), clause) => clause map {
        _ append **(v("entries"), p(lwm.group), s(SesameGroup.generateUri(UUID.fromString(values.head))))
      }
      case ((`supervisorAttribute`, values), clause) => clause map {
        _ append **(v("entries"), p(lwm.supervisor), s(User.generateUri(UUID.fromString(values.head))))
      }
      case ((`startAttribute`, values), clause) => clause map {
        _ append **(v("entries"), p(lwm.start), v("start")).filterStrStarts(v("start"), values.head)
      }
      case ((`endAttribute`, values), clause) => clause map {
        _ append **(v("entries"), p(lwm.end), v("end")).filterStrStarts(v("end"), values.head)
      }
      case ((`dateAttribute`, values), clause) => clause map {
        _ append **(v("entries"), p(lwm.date), v("date")).filterStrStarts(v("date"), values.head)
      }
      case ((`dateRangeAttribute`, values), clause) =>
        val split = values.head.split(",").toVector
        if (split.size != 2)
          Failure(new Throwable("A range can only have two parameters"))
        else
          clause map {
            _ append **(v("entries"), p(lwm.date), v("date")).filter(s"str(?date) >= '${split(0)}' && str(?date) <= '${split(1)}'")
          }

      case ((_, _), clause) => Failure(new Throwable("Unknown attribute"))
    } flatMap { clause =>
      val query = select distinct "entries" where clause

      repository.prepareQuery(query).
        select(_.get("entries")).
        transform(_.fold(List.empty[String])(_ map (_.stringValue()))).
        requestAll(repository.getMany[SesameScheduleEntry]).
        run
    }
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) action { implicit request =>
    val rebased = rebase(courseAttribute -> Seq(course))

    filter(rebased)(Set.empty)
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def allAtomicFrom(course: String) = restrictedContext(course)(GetAll) action { implicit request =>
    val rebased = rebase(courseAttribute -> Seq(course))

    filter(rebased)(Set.empty)
      .flatMap(set => retrieveLots[SesameScheduleEntryAtom](set map SesameScheduleEntry.generateUri))
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def allFromLabwork(course: String, labwork: String) = restrictedContext(course)(GetAll) action { implicit request =>
    val rebased = rebase(courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork))

    filter(rebased)(Set.empty)
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def allAtomicFromLabwork(course: String, labwork: String) = restrictedContext(course)(GetAll) action { implicit request =>
    val rebased = rebase(courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork))

    filter(rebased)(Set.empty)
      .flatMap(set => retrieveLots[SesameScheduleEntryAtom](set map SesameScheduleEntry.generateUri))
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def get(course: String, entry: String) = restrictedContext(course)(Get) action { request =>
    val uri = (UUID.fromString _ andThen SesameScheduleEntry.generateUri) (entry)

    retrieve[SesameScheduleEntry](uri)
      .mapResult(s => Ok(Json.toJson(s)).as(mimeType))
  }

  def getAtomic(course: String, entry: String) = restrictedContext(course)(Get) action { request =>
    val uri = (UUID.fromString _ andThen SesameScheduleEntry.generateUri) (entry)

    retrieve[SesameScheduleEntryAtom](uri)
      .mapResult(s => Ok(Json.toJson(s)).as(mimeType))
  }

  def update(course: String, entry: String) = restrictedContext(course)(Update) contentTypedAction { request =>
    validateInput(request)
      .when(_.id == UUID.fromString(entry),
        overwrite0)(
        BadRequest(Json.obj(
          "status" -> "KO",
          "message" -> s"Id found in body does not match id found in resource ($entry)"
        )))
      .mapResult(s => Ok(Json.toJson(s)).as(mimeType))
  }

  def updateAtomic(course: String, entry: String) = restrictedContext(course)(Update) contentTypedAction { request =>
    validateInput(request)
      .when(_.id == UUID.fromString(entry),
        overwrite0)(
        BadRequest(Json.obj(
          "status" -> "KO",
          "message" -> s"Id found in body does not match id found in resource ($entry)"
        )))
      .flatMap(s => retrieve[SesameScheduleEntryAtom](SesameScheduleEntry.generateUri(s)))
      .mapResult(s => Ok(Json.toJson(s)).as(mimeType))
  }

  def header = Action { implicit request =>
    NoContent.as(mimeType)
  }
}
