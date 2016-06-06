package controllers.reportCard

import java.util.UUID

import controllers.crud.{Atomic, SecureControllerContext, SessionChecking, _}
import controllers.reportCard.ReportCardEvaluationController._
import models.labwork._
import models.security.Permissions.{god, reportCardEvaluation}
import models.users.{Student, User}
import models.{Course, UriGenerator}
import modules.store.BaseNamespace
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Reads, Writes}
import play.api.mvc.{Controller, Result}
import services.{ReportCardServiceLike, RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.{CompositeClassUris, Descriptor}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
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
    with Atomic[ReportCardEvaluation]
    with Filterable[ReportCardEvaluation] {

  override implicit def reads: Reads[ReportCardEvaluation] = ReportCardEvaluation.reads

  override implicit def writes: Writes[ReportCardEvaluation] = ReportCardEvaluation.writes

  override implicit def uriGenerator: UriGenerator[ReportCardEvaluation] = ReportCardEvaluation

  override implicit def descriptor: Descriptor[Sesame, ReportCardEvaluation] = defaultBindings.ReportCardEvaluationDescriptor

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardEvaluationV1Json

  override protected def atomize(output: ReportCardEvaluation): Try[Option[JsValue]] = {
    import defaultBindings.ReportCardEvaluationAtomDescriptor
    import utils.Ops._
    import utils.Ops.MonadInstances.{optM, tryM}
    import ReportCardEvaluation.atomicWrites
    repository.get[ReportCardEvaluationAtom](ReportCardEvaluation.generateUri(output)) peek (Json.toJson(_))
  }

  def create(course: String, labwork: String) = createWith(course, labwork) { evals =>
    repository.addMany[ReportCardEvaluation](evals) map { _ =>
      Some(Created(Json.toJson(evals)).as(mimeType))
    }
  }

  def createAtomic(course: String, labwork: String) = createWith(course, labwork) { evals =>
    repository.addMany[ReportCardEvaluation](evals).flatMap(_ => atomizeMany(evals)) map { json =>
      Some(Created(json).as(mimeType))
    }
  }

  def all(course: String, labwork: String) = allWith(course, labwork) { evals =>
    Success(Some(Ok.chunked(chunkSimple(evals)).as(mimeType)))
  }

  def allAtomic(course: String, labwork: String) = allWith(course, labwork) { evals =>
    Success(Some(Ok.chunked(chunkAtoms(evals)).as(mimeType)))
  }

  def get(student: String) = getWith(student) { evals =>
    Success(Some(Ok.chunked(chunkSimple(evals)).as(mimeType)))
  }

  def getAtomic(student: String) = getWith(student) { evals =>
    Success(Some(Ok.chunked(chunkAtoms(evals)).as(mimeType)))
  }

  def preview(course: String, labwork: String) = previewWith(course, labwork) { evals =>
    Success(Some(Ok.chunked(chunkSimple(evals)).as(mimeType)))
  }

  def previewAtomic(course: String, labwork: String) = previewWith(course, labwork) { evals =>
    Success(Some(Ok.chunked(chunkAtoms(evals)).as(mimeType)))
  }

  private def evalsByService(labwork: String)(toResult: Set[ReportCardEvaluation] => Try[Option[Result]]) = {
    import defaultBindings.{AssignmentPlanDescriptor, ReportCardEntryDescriptor}
    import utils.Ops.MonadInstances.tryM
    import utils.Ops.TraverseInstances.travO
    import utils.Ops._
    import store.sparql.select
    import store.sparql.select._

    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]

    val labworkId = UUID.fromString(labwork)
    val cardsQuery = select ("cards") where {
      **(v("cards"), p(rdf.`type`), s(lwm.ReportCardEntry)).
        **(v("cards"), p(lwm.labwork), s(Labwork.generateUri(labworkId)))
    }

    val cardsPrepared = repository.prepareQuery(cardsQuery).
      select(_.get("cards")).
      transform(_.fold(List.empty[Value])(vs => vs)).
      requestAll[Set, ReportCardEntry](vs => repository.getMany[ReportCardEntry](vs.map(_.stringValue)))

    val result = for {
      assignmentPlan <- repository.getAll[AssignmentPlan].map(_.find(_.labwork == labworkId)) // TODO query does not work, there are two objects to expand
      cards <- cardsPrepared.run
      optEvals = assignmentPlan.map(ap => reportCardService.evaluate(ap, cards))
      result <- optEvals.map(toResult).sequenceM
    } yield result.flatten

    handleResult(result)
  }

  private def evalsByRepo(filter: Map[String, Seq[String]])(toResult: Set[ReportCardEvaluation] => Try[Option[Result]]) = {
    val result = getWithFilter(filter)(Set.empty) flatMap toResult
    handleResult(result)
  }

  private def previewWith(course: String, labwork: String)(toResult: Set[ReportCardEvaluation] => Try[Option[Result]]) = restrictedContext(course)(Create) action { implicit request =>
    evalsByService(labwork)(toResult)
  }

  private def createWith(course: String, labwork: String)(toResult: Set[ReportCardEvaluation] => Try[Option[Result]]) = restrictedContext(course)(Create) contentTypedAction { implicit request =>
    evalsByService(labwork)(toResult)
  }

  private def allWith(course: String, labwork: String)(toResult: Set[ReportCardEvaluation] => Try[Option[Result]]) = restrictedContext(course)(GetAll) action { implicit request =>
    evalsByRepo(Map(courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork)))(toResult)
  }

  private def getWith(student: String)(toResult: Set[ReportCardEvaluation] => Try[Option[Result]]) = contextFrom(Get) action { implicit request =>
    evalsByRepo(Map(studentAttribute -> Seq(student)))(toResult)
  }

  private def handleResult(result: Try[Option[Result]]) = result match {
    case Success(s) =>
      s match {
        case Some(r) =>
          r
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

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, reportCardEvaluation.create)
    case Get => SecureBlock(restrictionId, reportCardEvaluation.get)
    case GetAll => SecureBlock(restrictionId, reportCardEvaluation.getAll)
    case _ => PartialSecureBlock(god)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(reportCardEvaluation.get)
    case _ => PartialSecureBlock(god)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[ReportCardEvaluation]): Try[Set[ReportCardEvaluation]] = {
    import store.sparql.select
    import store.sparql.select._

    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]

    queryString.foldLeft(Try(**(v("entries"), p(rdf.`type`), s(lwm.ReportCardEvaluation)))) {
      case (clause, (`courseAttribute`, courses)) => clause map {
        _ append **(v("entries"), p(lwm.labwork), v("labwork")).**(v("labwork"), p(lwm.course), s(Course.generateUri(UUID.fromString(courses.head))))
      }
      case (clause, (`labworkAttribute`, labworks)) => clause map {
        _ append **(v("entries"), p(lwm.labwork), s(Labwork.generateUri(UUID.fromString(labworks.head))))
      }
      case (clause, (`studentAttribute`, students)) => clause map {
        _ append **(v("entries"), p(lwm.student), s(User.generateUri(UUID.fromString(students.head))))
      }
      case _ => Failure(new Throwable("Unknown attribute"))
    } flatMap { clause =>
      val query = select distinct "entries" where clause

      repository.prepareQuery(query).
        select(_.get("entries")).
        transform(_.fold(List.empty[String])(_.map(_.stringValue))).
        requestAll(repository.getMany(_)).
        run
    }
  }
}
