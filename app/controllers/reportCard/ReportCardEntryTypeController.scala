package controllers.reportCard

import java.util.UUID

import controllers.crud._
import models.UriGenerator
import models.labwork.{ReportCardEntry, Labwork, ReportCardEntryType}
import models.security.Permissions._
import models.users.User
import modules.store.BaseNamespace
import org.joda.time.LocalDate
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsError, Json, Reads, Writes}
import play.api.mvc.Controller
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.util.{Try, Failure, Success}

object ReportCardEntryTypeController {
  val studentAttribute = "student"
  val labworkAttribute = "labwork"
  val dateAttribute = "date"
  val startAttribute = "start"
  val endAttribute = "end"
}

class ReportCardEntryTypeController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends Controller
  with BaseNamespace
  with JsonSerialisation[ReportCardEntryType, ReportCardEntryType]
  with SesameRdfSerialisation[ReportCardEntryType]
  with ContentTyped
  with Secured
  with SessionChecking
  with SecureControllerContext {

  override implicit def reads: Reads[ReportCardEntryType] = ReportCardEntryType.reads

  override implicit def writes: Writes[ReportCardEntryType] = ReportCardEntryType.writes

  override implicit def rdfReads: FromPG[Sesame, ReportCardEntryType] = defaultBindings.ReportCardEntryTypeBinding.reportCardEntryTypeBinding

  override implicit def classUrisFor: ClassUrisFor[Sesame, ReportCardEntryType] = defaultBindings.ReportCardEntryTypeBinding.classUri

  override implicit def uriGenerator: UriGenerator[ReportCardEntryType] = ReportCardEntryType

  override implicit def rdfWrites: ToPG[Sesame, ReportCardEntryType] = defaultBindings.ReportCardEntryTypeBinding.reportCardEntryTypeBinding

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardEntryTypeV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Update => SecureBlock(restrictionId, reportCardEntryType.update)
    case GetAll => SecureBlock(restrictionId, reportCardEntryType.getAll)
    case _ => PartialSecureBlock(god)
  }

  def update(course: String, cardEntry: String, entryType: String) = restrictedContext(course)(Update) contentTypedAction { request =>
    request.body.validate[ReportCardEntryType].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => {
        success.id == UUID.fromString(entryType) match {
          case true =>
            repository.update(success) match {
              case Success(_) =>
                Ok(Json.toJson(success)).as(mimeType)
              case Failure(e) =>
                InternalServerError(Json.obj(
                  "status" -> "KO",
                  "errors" -> e.getMessage
                ))
            }
          case false =>
            BadRequest(Json.obj(
              "status" -> "KO",
              "message" -> s"Update body does not match with ReportCardEntryType (${success.id})"
            ))
        }

      }
    )
  }

  def all(course: String) = restrictedContext(course)(GetAll) action { request =>
    import store.sparql.select._
    import store.sparql.select
    import defaultBindings.ReportCardEntryBinding.reportCardEntryBinding
    import ReportCardEntryTypeController._
    import utils.Ops.MonadInstances.setM

    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]
    implicit val ns = repository.namespace

    if (request.queryString.isEmpty)
      BadRequest(Json.obj(
        "status" -> "KO",
        "message" -> "Request should contain at least one attribute"
      ))
    else
      request.queryString.foldLeft(Try(^(v("entries"), p(rdf.`type`), s(lwm.ReportCardEntry)))) {
        case (clause, (`studentAttribute`, values)) => clause map {
          _ append ^(v("entries"), p(lwm.student), s(User.generateUri(UUID.fromString(values.head))))
        }
        case (clause, (`labworkAttribute`, values)) => clause map {
          _ append ^(v("entries"), p(lwm.labwork), s(Labwork.generateUri(UUID.fromString(values.head))))
        }
        case (clause, (`dateAttribute`, values)) => clause map {
          _ append ^(v("entries"), p(lwm.localDate), o(values.head))
        }
        case (clause, (`startAttribute`, values)) => clause map {
          _ append ^(v("entries"), p(lwm.start), o(values.head))
        }
        case (clause, (`endAttribute`, values)) => clause map {
          _ append ^(v("entries"), p(lwm.end), o(values.head))
        }
        case _ => Failure(new Throwable("Unknown attribute"))
      } flatMap { clause =>
        val query = select distinct "entries" where clause
        println(query.run)
        repository.prepareQuery(query).
          select(_.get("entries")).
          transform(_.fold(List.empty[Value])(identity)).
          requestAll[Set, ReportCardEntry](values => repository.getMany[ReportCardEntry](values.map(_.stringValue))).
          flatMap(_.entryTypes).
          run

      } match {
        case Success(entries) =>
          Ok(Json.toJson(entries)).as(mimeType)
        case Failure(e) =>
          InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> e.getMessage
          ))
      }
  }
}
