package controllers

import java.util.UUID

import models.Permissions.{god, reportCardEntryType}
import models.{ReportCardEntryType, UriGenerator}
import modules.BaseNamespace
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.{Action, Controller}
import services.{RoleService, SessionHandlingService}
import store.bind.Descriptor.Descriptor
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Success, Try}

// TODO inherit from AbstractCRUDController
class ReportCardEntryTypeController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService)
  extends Controller
    with BaseNamespace
    with JsonSerialisation[ReportCardEntryType, ReportCardEntryType, ReportCardEntryType]
    with RdfSerialisation[ReportCardEntryType, ReportCardEntryType]
    with ContentTyped
    with Secured
    with SessionChecking
    with SecureControllerContext
    with Stored
    with Consistent[ReportCardEntryType, ReportCardEntryType]
    with ModelConverter[ReportCardEntryType, ReportCardEntryType]
    with Filterable[ReportCardEntryType]
    with Basic[ReportCardEntryType, ReportCardEntryType, ReportCardEntryType] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardEntryTypeV1Json

  override implicit val descriptor: Descriptor[Sesame, ReportCardEntryType] = defaultBindings.ReportCardEntryTypeDescriptor

  override val descriptorAtom: Descriptor[Sesame, ReportCardEntryType] = descriptor

  override implicit val reads: Reads[ReportCardEntryType] = ReportCardEntryType.reads

  override implicit val writes: Writes[ReportCardEntryType] = ReportCardEntryType.writes

  override val writesAtom: Writes[ReportCardEntryType] = ReportCardEntryType.writesAtom

  override implicit val uriGenerator: UriGenerator[ReportCardEntryType] = ReportCardEntryType

  override protected def compareModel(input: ReportCardEntryType, output: ReportCardEntryType): Boolean = input == output

  override protected def fromInput(input: ReportCardEntryType, existing: Option[ReportCardEntryType]): ReportCardEntryType = input

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Update => SecureBlock(restrictionId, reportCardEntryType.update)
    case _ => PartialSecureBlock(god)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[ReportCardEntryType]): Try[Set[ReportCardEntryType]] = Success(all)

  def update(course: String, entryType: String) = restrictedContext(course)(Update) contentTypedAction { request =>
    validate(request)
      .when(_.id == UUID.fromString(entryType), overwrite0)(
        BadRequest(Json.obj(
          "status" -> "KO",
          "message" -> s"Id found in body does not match id found in resource ($entryType)"
        ))
      )
      .mapResult(entry => Ok(Json.toJson(entry)).as(mimeType))
  }

  def header = Action { implicit request =>
    NoContent.as(mimeType)
  }
}
