package controllers.reportCard

import java.util.UUID

import controllers.crud.{SecureControllerContext, SessionChecking, _}
import controllers.reportCard.ReportCardEvaluationController._
import models.labwork._
import models.security.Permissions.{god, reportCardEvaluation}
import models.users.{Student, User}
import models.{Course, UriGenerator}
import modules.store.BaseNamespace
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.Controller
import services.{ReportCardServiceLike, RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.{Namespace, SemanticUtils, SesameRepository}
import utils.{Attempt, Return, Continue, LwmMimeType}

import scala.collection.Map
import scala.util.{Failure, Success, Try}

object ReportCardEvaluationController {
  val courseAttribute = "course"
  val labworkAttribute = "labwork"
  val studentAttribute = "student"
}

class ReportCardEvaluationController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService, val reportCardService: ReportCardServiceLike) extends AbstractCRUDController[ReportCardEvaluation, ReportCardEvaluation, ReportCardEvaluationAtom]{

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardEvaluationV1Json

  override implicit val descriptor: Descriptor[Sesame, ReportCardEvaluation] = defaultBindings.ReportCardEvaluationDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, ReportCardEvaluationAtom] = defaultBindings.ReportCardEvaluationAtomDescriptor

  override implicit val reads: Reads[ReportCardEvaluation] = ReportCardEvaluation.reads

  override implicit val writes: Writes[ReportCardEvaluation] = ReportCardEvaluation.writes

  override implicit val writesAtom: Writes[ReportCardEvaluationAtom] = ReportCardEvaluation.writesAtom

  override implicit val uriGenerator: UriGenerator[ReportCardEvaluation] = ReportCardEvaluation

  override protected def compareModel(input: ReportCardEvaluation, output: ReportCardEvaluation): Boolean = false

  override protected def fromInput(input: ReportCardEvaluation, existing: Option[ReportCardEvaluation]): ReportCardEvaluation = input

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
    import utils.Ops.MonadInstances.listM

    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

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

      repository.prepareQuery(query)
        .select(_.get("entries"))
        .transform(_.fold(List.empty[Value])(identity))
        .map(_.stringValue())
        .requestAll(repository.getMany[ReportCardEvaluation](_))
        .run
    }
  }

  /*def create(course: String, labwork: String) = restrictedContext(course)(Create) contentTypedAction { request =>
    evaluate(labwork)
      .flatMap(set => addLots(set.toList))
      .mapResult(evals => Created(Json.toJson(evals)).as(mimeType))
  }

  def createAtomic(course: String, labwork: String) = restrictedContext(course)(Create) contentTypedAction { request =>
    evaluate(labwork)
      .flatMap(set => addLots(set.toList))
      .flatMap(list => retrieveLots[ReportCardEvaluationAtom](list map ReportCardEvaluation.generateUri))
      .mapResult(evals => Created(Json.toJson(evals)).as(mimeType))
  }*/

  def allForm(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    all(NonSecureBlock)(rebase(courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork)))
  }

  /*def all(course: String, labwork: String) = restrictedContext(course)(GetAll) action { implicit request =>
    val rebased = rebase(ReportCardEvaluation.generateBase, courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork))
    filtered(rebased)(Set.empty)
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def allAtomic(course: String, labwork: String) = restrictedContext(course)(GetAll) action { implicit request =>
    val rebased = rebase(ReportCardEvaluation.generateBase, courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork))
    filtered(rebased)(Set.empty)
      .flatMap(set => retrieveLots[ReportCardEvaluationAtom](set map ReportCardEvaluation.generateUri))
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def get(student: String) = contextFrom(Get) action { implicit request =>
    val rebased = rebase(ReportCardEvaluation.generateBase, studentAttribute -> Seq(student))
    filtered(rebased)(Set.empty)
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def getAtomic(student: String) = contextFrom(Get) action { implicit request =>
    val rebased = rebase(ReportCardEvaluation.generateBase, studentAttribute -> Seq(student))
    filtered(rebased)(Set.empty)
      .flatMap(set => retrieveLots[ReportCardEvaluationAtom](set map ReportCardEvaluation.generateUri))
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }*/

  def preview(course: String, labwork: String) = restrictedContext(course)(Create) action { request =>
    evaluate(labwork)
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def previewAtomic(course: String, labwork: String) = restrictedContext(course)(Create) action { request =>
    evaluate(labwork)
      .flatMap(atomic)
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def evaluate(labwork: String): Attempt[Set[ReportCardEvaluation]] = {
    import defaultBindings.{AssignmentPlanDescriptor, ReportCardEntryDescriptor}
    import store.sparql.select
    import store.sparql.select._

    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]

    val labworkId = UUID.fromString(labwork)
    val cardsQuery = select("cards") where {
      **(v("cards"), p(rdf.`type`), s(lwm.ReportCardEntry)).
        **(v("cards"), p(lwm.labwork), s(Labwork.generateUri(labworkId)))
    }

    val cardsPrepared = repository.prepareQuery(cardsQuery).
      select(_.get("cards")).
      transform(_.fold(List.empty[Value])(vs => vs)).
      requestAll[Set, ReportCardEntry](vs => repository.getMany[ReportCardEntry](vs.map(_.stringValue)))

    optional {
      for {
        assignmentPlan <- repository.getAll[AssignmentPlan].map(_.find(_.labwork == labworkId)) // TODO query does not work, there are two objects to expand
        cards <- cardsPrepared.run
      } yield assignmentPlan.map(ap => reportCardService.evaluate(ap, cards))
    }
  }

  def atomic(evals: Set[ReportCardEvaluation]): Attempt[Set[ReportCardEvaluationAtom]] = {
    import defaultBindings.{LabworkDescriptor, StudentDescriptor}

    SemanticUtils.collect {
      evals map { eval =>
        for {
          optLabwork <- repository.get[Labwork](Labwork.generateUri(eval.labwork))
          optStudent <- repository.get[Student](User.generateUri(eval.student))
        } yield for {
          l <- optLabwork; s <- optStudent
        } yield ReportCardEvaluationAtom(s, l, eval.label, eval.bool, eval.int, eval.invalidated, eval.id)
      }
    } match {
      case Success(set) => Continue(set)
      case Failure(e) => Return(InternalServerError(
        Json.obj(
          "status" -> "KO",
          "errors" -> s"Error while generating groups for labwork: ${e.getMessage}"
        )))
    }
  }

  override protected def coAtomic(atom: ReportCardEvaluationAtom): ReportCardEvaluation = ReportCardEvaluation(
    atom.student.id,
    atom.labwork.id,
    atom.label,
    atom.bool,
    atom.int,
    atom.invalidated,
    atom.id
  )
}
