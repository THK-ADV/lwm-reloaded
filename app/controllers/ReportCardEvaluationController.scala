package controllers

import java.util.UUID

import models.Permissions.{god, reportCardEvaluation}
import models._
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import services.{ReportCardServiceLike, RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.{Namespace, SemanticUtils, SesameRepository}
import utils.{Attempt, Continue, LwmMimeType, Return}
import controllers.ReportCardEvaluationController._
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

  def createFrom(course: String, labwork: String) = restrictedContext(course)(Create) contentTypedAction { implicit request =>
    val result = for {
      existing <- filter(rebase(labworkAttribute -> Seq(labwork)))(Set.empty)
      newest <- evaluate(labwork)
      newestEvaluated = reportCardService.removeThoseWhoAlreadyPassed(existing, newest)
      toDelete = existing.filter(e => newestEvaluated.exists(_.student == e.student))
      _ <- removeLots[ReportCardEvaluation](toDelete map ReportCardEvaluation.generateUri)
      added <- addLots(newestEvaluated)
    } yield chunk(added.toSet)

    result.mapResult(enum => Created.stream(enum).as(mimeType))
  }

  def createAtomicFrom(course: String, labwork: String) = restrictedContext(course)(Create) contentTypedAction { implicit request =>
    val result = for {
      existing <- filter(rebase(labworkAttribute -> Seq(labwork)))(Set.empty)
      newest <- evaluate(labwork)
      newestEvaluated = reportCardService.removeThoseWhoAlreadyPassed(existing, newest)
      toDelete = existing.filter(e => newestEvaluated.exists(_.student == e.student))
      _ <- removeLots[ReportCardEvaluation](toDelete map ReportCardEvaluation.generateUri)
      added <- addLots(newestEvaluated)
      atoms <- atomic(added.toSet)
    } yield chunk(atoms)

    result.mapResult(enum => Created.stream(enum).as(mimeType))
  }

  def createForStudent(course: String, labwork: String, student: String) = restrictedContext(course)(Create) contentTypedAction { implicit request =>
    filter(rebase(labworkAttribute -> Seq(labwork), studentAttribute -> Seq(student)))(Set.empty).
      when(evals => evals.isEmpty || evals.size == ReportCardEntryType.all.size, evals => removeLots[ReportCardEvaluation](evals map ReportCardEvaluation.generateUri)) {
        PreconditionFailed(Json.obj(
          "status" -> "KO",
          "message" -> s"More than ${ReportCardEntryType.all.size} reportcard evaluations found for $student"
        ))
      }.map(_ => reportCardService.evaluateExplicit(UUID.fromString(student), UUID.fromString(labwork))).
      flatMap(addLots).
      map(evals => chunk(evals.toSet)).
      mapResult(enum => Created.stream(enum).as(mimeType))
  }

  def createAtomicForStudent(course: String, labwork: String, student: String) = restrictedContext(course)(Create) contentTypedAction { implicit request =>
    filter(rebase(labworkAttribute -> Seq(labwork), studentAttribute -> Seq(student)))(Set.empty).
      when(evals => evals.isEmpty || evals.size == ReportCardEntryType.all.size, evals => removeLots[ReportCardEvaluation](evals map ReportCardEvaluation.generateUri)) {
        PreconditionFailed(Json.obj(
          "status" -> "KO",
          "message" -> s"More than ${ReportCardEntryType.all.size} reportcard evaluations found for $student"
        ))
      }.map(_ => reportCardService.evaluateExplicit(UUID.fromString(student), UUID.fromString(labwork))).
      flatMap(addLots).
      map(_.toSet).
      flatMap(atomic).
      map(atoms => chunk(atoms)).
      mapResult(enum => Created.stream(enum).as(mimeType))
  }

  def allFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) action { implicit request =>
    filter(rebase(courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork)))(Set.empty)
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def allAtomicFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) action { implicit request =>
    filter(rebase(courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork)))(Set.empty)
      .flatMap(set => retrieveLots[ReportCardEvaluationAtom](set map ReportCardEvaluation.generateUri))
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def get(student: String) = contextFrom(Get) action { implicit request =>
    filter(rebase(studentAttribute -> Seq(student)))(Set.empty)
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

  def getAtomic(student: String) = contextFrom(Get) action { implicit request =>
    filter(rebase(studentAttribute -> Seq(student)))(Set.empty)
      .flatMap(set => retrieveLots[ReportCardEvaluationAtom](set map ReportCardEvaluation.generateUri))
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }

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

  private def evaluate(labwork: String): Attempt[Set[ReportCardEvaluation]] = {
    import defaultBindings.{AssignmentPlanDescriptor, ReportCardEntryDescriptor}
    import store.sparql.select
    import store.sparql.select._

    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    val labworkId = UUID.fromString(labwork)

    def assignmentPlan = {
      import utils.Ops.MonadInstances.optM
      import utils.Ops.NaturalTrasformations.identity
      import utils.Ops.TraverseInstances.travO

      val apQuery = select("ap") where {
        **(v("ap"), p(rdf.`type`), s(lwm.AssignmentPlan)).
          **(v("ap"), p(lwm.labwork), s(Labwork.generateUri(labworkId)))
      }

      repository.prepareQuery(apQuery).
        select(_.get("ap")).
        changeTo(_.headOption).
        map(_.stringValue)(optM).
        request(repository.get[AssignmentPlan])
    }

    def reportCards = {
      import utils.Ops.MonadInstances.listM

      val cardsQuery = select("cards") where {
        **(v("cards"), p(rdf.`type`), s(lwm.ReportCardEntry)).
          **(v("cards"), p(lwm.labwork), s(Labwork.generateUri(labworkId)))
      }

      repository.prepareQuery(cardsQuery).
        select(_.get("cards")).
        transform(_.fold(List.empty[Value])(identity)).
        map(_.stringValue).
        requestAll[Set, ReportCardEntry](repository.getMany[ReportCardEntry])
    }

    optional {
      for {
        plan <- assignmentPlan.run
        cards <- reportCards.run
      } yield plan.map(reportCardService.evaluate(_, cards))
    }
  }

  private def atomic(evals: Set[ReportCardEvaluation]): Attempt[Set[ReportCardEvaluationAtom]] = {
    import defaultBindings.{LabworkAtomDescriptor, StudentDescriptor}

    SemanticUtils.collect {
      evals map { eval =>
        for {
          optLabwork <- repository.get[LabworkAtom](Labwork.generateUri(eval.labwork))
          optStudent <- repository.get[Student](User.generateUri(eval.student))
        } yield for {
          l <- optLabwork; s <- optStudent
        } yield ReportCardEvaluationAtom(s, l, eval.label, eval.bool, eval.int, eval.timestamp, eval.invalidated, eval.id)
      }
    } match {
      case Success(set) => Continue(set)
      case Failure(e) => Return(
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }

  override protected def coAtomic(atom: ReportCardEvaluationAtom): ReportCardEvaluation = ReportCardEvaluation(
    atom.student.id,
    atom.labwork.id,
    atom.label,
    atom.bool,
    atom.int,
    atom.timestamp,
    atom.invalidated,
    atom.id
  )
}
