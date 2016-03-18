package controllers

import java.util.UUID

import controllers.crud._
import models.users.User
import models.{ReportCardEntryType, UriGenerator, ReportCard}
import modules.store.BaseNamespace
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{FromPG, ClassUrisFor, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import play.api.mvc._
import models.security.Permissions._
import scala.util.{Failure, Success, Try}

class ReportCardController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends Controller
  with BaseNamespace
  with JsonSerialisation[ReportCard, ReportCard]
  with SesameRdfSerialisation[ReportCard]
  with Atomic[ReportCard]
  with ContentTyped
  with Secured
  with SessionChecking
  with SecureControllerContext {

  override implicit def rdfReads: FromPG[Sesame, ReportCard] = defaultBindings.ReportCardBinding.reportCardBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, ReportCard] = defaultBindings.ReportCardBinding.classUri

  override implicit def uriGenerator: UriGenerator[ReportCard] = ReportCard

  override implicit def rdfWrites: ToPG[Sesame, ReportCard] = defaultBindings.ReportCardBinding.reportCardBinder

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardV1Json

  override protected def atomize(output: ReportCard): Try[Option[JsValue]] = Success(Some(Json.toJson(output)))

  override implicit def reads: Reads[ReportCard] = ReportCard.reads

  override implicit def writes: Writes[ReportCard] = ReportCard.writes

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(reportCard.get)
    case _ => PartialSecureBlock(god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Update => SecureBlock(restrictionId, reportCard.update)
    case _ => PartialSecureBlock(god)
  }

  // TODO TEST
  def get(student: String)= contextFrom(Get) action { request =>
    import utils.Ops.MonadInstances.optM
    import utils.Ops.NaturalTrasformations._
    import utils.Ops.TraverseInstances.travO
    import store.sparql.select
    import store.sparql.select._
    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    val query = select ("card") where {
      ^(v("card"), p(rdf.`type`), s(lwm.ReportCard)).
      ^(v("card"), p(lwm.student), s(User.generateUri(UUID.fromString(student))(namespace)))
    }

    val result = repository.prepareQuery(query).
      select(_.get("card")).
      changeTo(_.headOption).
      map(_.stringValue)(optM).
      request[Option, ReportCard](repository.get[ReportCard]).
      run

    result match {
      case Success(s) =>
        s match {
          case Some(card) =>
            Ok(Json.toJson(card))
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

  def updateReportCardEntryType(course: String, card: String, cardEntry: String) = restrictedContext(course)(Update) contentTypedAction { request =>
    import ReportCardEntryType._
    import defaultBindings.ReportCardEntryTypeBinding.reportCardEntryTypeBinding

    request.body.validate[ReportCardEntryType].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => {
        repository.update(success)(reportCardEntryTypeBinding, ReportCardEntryType) match {
          case Success(s) =>
            Ok(Json.obj(
              "reportCardId" -> card,
              "reportCardEntryId" -> cardEntry,
              "assignmentEntryType" -> Json.toJson(success)
            ))
          case Failure(e) =>
            InternalServerError(Json.obj(
              "status" -> "KO",
              "errors" -> e.getMessage
            ))
        }
      }
    )
  }
}
