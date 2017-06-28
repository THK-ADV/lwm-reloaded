package controllers

import java.util.UUID

import models.Permissions.{god, reportCardEntryType}
import models.{SesameReportCardEntryType, UriGenerator}
import modules.BaseNamespace
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.{Action, Controller}
import services.{RoleServiceLike, SessionHandlingService}
import store.bind.Descriptor.Descriptor
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.util.{Success, Try}

// TODO inherit from AbstractCRUDController
class ReportCardEntryTypeController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleServiceLike)
  extends Controller
    with BaseNamespace
    with JsonSerialisation[SesameReportCardEntryType, SesameReportCardEntryType, SesameReportCardEntryType]
    with RdfSerialisation[SesameReportCardEntryType, SesameReportCardEntryType]
    with ContentTyped
    with Secured
    with SessionChecking
    with SecureControllerContext
    with Stored
    with Consistent[SesameReportCardEntryType, SesameReportCardEntryType]
    with ModelConverter[SesameReportCardEntryType, SesameReportCardEntryType]
    with Filterable[SesameReportCardEntryType]
    with Basic[SesameReportCardEntryType, SesameReportCardEntryType, SesameReportCardEntryType] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardEntryTypeV1Json

  override implicit val descriptor: Descriptor[Sesame, SesameReportCardEntryType] = defaultBindings.ReportCardEntryTypeDescriptor

  override val descriptorAtom: Descriptor[Sesame, SesameReportCardEntryType] = descriptor

  override implicit val reads: Reads[SesameReportCardEntryType] = SesameReportCardEntryType.reads

  override implicit val writes: Writes[SesameReportCardEntryType] = SesameReportCardEntryType.writes

  override val writesAtom: Writes[SesameReportCardEntryType] = SesameReportCardEntryType.writesAtom

  override implicit val uriGenerator: UriGenerator[SesameReportCardEntryType] = SesameReportCardEntryType

  override protected def compareModel(input: SesameReportCardEntryType, output: SesameReportCardEntryType): Boolean = input == output

  override protected def fromInput(input: SesameReportCardEntryType, existing: Option[SesameReportCardEntryType]): SesameReportCardEntryType = input

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Update => SecureBlock(restrictionId, reportCardEntryType.update)
    case _ => PartialSecureBlock(god)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[SesameReportCardEntryType]): Try[Set[SesameReportCardEntryType]] = Success(all)

  def update(course: String, entryType: String) = restrictedContext(course)(Update) contentTypedAction { request =>
    validateInput(request)
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
