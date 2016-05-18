package controllers.reportCard

import java.util.UUID

import controllers.crud.{Atomic, SecureControllerContext, SessionChecking, _}
import models.UriGenerator
import models.labwork._
import modules.store.BaseNamespace
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Reads, Writes}
import play.api.mvc.{Controller, Result}
import services.{ReportCardServiceLike, RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import models.security.Permissions.{god, reportCardEvaluation}
import models.users.{Student, User}
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import store.Prefixes.LWMPrefix

import scala.util.{Failure, Success, Try}

object ReportCardEvaluationController {
  val courseAttribute = "course"
  val labworkAttribute = "labwork"
  val studentAttribute = "student"
}

class ReportCardEvaluationController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService, val reportCardService: ReportCardServiceLike)
  extends Controller
    with BaseNamespace
    with JsonSerialisation[ReportCardEvaluation, ReportCardEvaluation]
    with SesameRdfSerialisation[ReportCardEvaluation]
    with ContentTyped
    with Chunkable[ReportCardEvaluation]
    with Secured
    with SessionChecking
    with SecureControllerContext
    with Atomic[ReportCardEvaluation] {

  override implicit def reads: Reads[ReportCardEvaluation] = ReportCardEvaluation.reads

  override implicit def writes: Writes[ReportCardEvaluation] = ReportCardEvaluation.writes

  override implicit def rdfReads: FromPG[Sesame, ReportCardEvaluation] = defaultBindings.ReportCardEvaluationBinding.reportCardEvaluationBinding

  override implicit def classUrisFor: ClassUrisFor[Sesame, ReportCardEvaluation] = defaultBindings.ReportCardEvaluationBinding.classUri

  override implicit def uriGenerator: UriGenerator[ReportCardEvaluation] = ReportCardEvaluation

  override implicit def rdfWrites: ToPG[Sesame, ReportCardEvaluation] = defaultBindings.ReportCardEvaluationBinding.reportCardEvaluationBinding

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardEvaluationV1Json

  override protected def atomize(output: ReportCardEvaluation): Try[Option[JsValue]] = {
    import defaultBindings.StudentBinding.studentBinder
    import defaultBindings.LabworkBinding.labworkBinder
    import ReportCardEvaluation.atomicWrites

    for {
      student <- repository.get[Student](User.generateUri(output.student))
      labwork <- repository.get[Labwork](Labwork.generateUri(output.labwork))
    } yield for {
      s <- student; l <- labwork
    } yield Json.toJson(
      ReportCardEvaluationAtom(s, l, output.label, output.bool, output.int, output.id)
    )
  }

  def preview(course: String, labwork: String) = previewWith(course, labwork) { evals =>
    Ok.chunked(chunkSimple(evals)).as(mimeType)
  }

  def previewAtomic(course: String, labwork: String) = previewWith(course, labwork) { evals =>
    Ok.chunked(chunkAtoms(evals)).as(mimeType)
  }

  private def previewWith(course: String, labwork: String)(toResult: Set[ReportCardEvaluation] => Result) = restrictedContext(course)(Create) action { implicit request =>
    import defaultBindings.AssignmentPlanBinding._
    import defaultBindings.ReportCardEntryBinding.reportCardEntryBinder
    import utils.Ops.MonadInstances.optM
    import utils.Ops.TraverseInstances.travO
    import utils.Ops.NaturalTrasformations._

    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]

    def query(variable: String, subject: repository.Rdf#URI) = {
      import store.sparql.select
      import store.sparql.select._

      select (variable) where {
        **(v(variable), p(rdf.`type`), s(subject)).
          **(v(variable), p(lwm.labwork), s(Labwork.generateUri(UUID.fromString(labwork))))
      }
    }

    /*val apQuery = repository.prepareQuery(a).
      select(_.get("ap")).
      transform(_.fold(List.empty[Value])(vs => vs)).transform(_.headOption).
      request[Option, AssignmentPlan](v => repository.getExpanded[AssignmentPlan](v.stringValue))*/

    val cardsQuery = repository.prepareQuery(query("cards", lwm.ReportCardEntry)).
      select(_.get("cards")).
      transform(_.fold(List.empty[Value])(vs => vs)).
      requestAll(vs => repository.getManyExpanded[ReportCardEntry](vs.map(_.stringValue)))

    (for {
      assignmentPlan <- repository.get[AssignmentPlan].map(_.find(_.labwork == UUID.fromString(labwork)))
      cards <- cardsQuery.run
    } yield assignmentPlan.map { ap =>
      reportCardService.evaluate(ap, cards)
    }) match {
      case Success(s) =>
        s match {
          case Some(evals) => toResult(evals)
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

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    /*case Create => SecureBlock(restrictionId, reportCardEvaluation.create)
    case Get => SecureBlock(restrictionId, reportCardEvaluation.get)
    case GetAll => SecureBlock(restrictionId, reportCardEvaluation.getAll)
    case _ => PartialSecureBlock(god)*/
    case _ => NonSecureBlock
  }
}
