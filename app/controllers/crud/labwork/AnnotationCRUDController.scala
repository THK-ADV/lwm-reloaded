package controllers.crud.labwork

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.labwork._
import models.users.{User, Student}
import org.w3.banana.binder.{FromPG, ClassUrisFor, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, JsValue, Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import models.security.Permissions._
import scala.collection.Map
import scala.util.{Failure, Try}
import utils.RequestOps._
import AnnotationCRUDController._

object AnnotationCRUDController {
  val labworkAttribute = "labwork"
  val studentAttribute = "student"
  val reportCardEntryAttribute = "reportCardEntry"
}

class AnnotationCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[AnnotationProtocol, Annotation] {

  override implicit def reads: Reads[AnnotationProtocol] = Annotation.reads

  override implicit def writes: Writes[Annotation] = Annotation.writes

  override implicit def rdfReads: FromPG[Sesame, Annotation] = defaultBindings.AnnotationBinding.annotationBinding

  override implicit def classUrisFor: ClassUrisFor[Sesame, Annotation] = defaultBindings.AnnotationBinding.classUri

  override implicit def uriGenerator: UriGenerator[Annotation] = Annotation

  override implicit def rdfWrites: ToPG[Sesame, Annotation] = defaultBindings.AnnotationBinding.annotationBinding

  override implicit val mimeType: LwmMimeType = LwmMimeType.annotationV1Json

  override protected def compareModel(input: AnnotationProtocol, output: Annotation): Boolean = input.message == output.message

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Annotation]): Try[Set[Annotation]] = {
    queryString.foldRight(Try[Set[Annotation]](all)) {
      case ((`labworkAttribute`, values), t) => t flatMap (set => Try(UUID.fromString(values.head)).map(id => set.filter(_.labwork == id)))
      case ((`studentAttribute`, values), t) => t flatMap (set => Try(UUID.fromString(values.head)).map(id => set.filter(_.student == id)))
      case ((`reportCardEntryAttribute`, values), t) => t flatMap (set => Try(UUID.fromString(values.head)).map(id => set.filter(_.reportCardEntry == id)))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def atomize(output: Annotation): Try[Option[JsValue]] = {
    import defaultBindings.StudentBinding.studentBinder
    import defaultBindings.LabworkBinding.labworkBinder
    import defaultBindings.ReportCardEntryBinding.reportCardEntryBinder
    import Annotation.atomicWrites

    for {
      student <- repository.get[Student](User.generateUri(output.student)(namespace))
      labwork <- repository.get[Labwork](Labwork.generateUri(output.labwork)(namespace))
      reportCardEntry <-repository.get[ReportCardEntry](ReportCardEntry.generateUri(output.reportCardEntry)(namespace))
    } yield for {
      s <- student; l <- labwork; e <- reportCardEntry
    } yield Json.toJson(AnnotationAtom(s, l, e, output.message, output.timestamp, output.id))
  }

  override protected def fromInput(input: AnnotationProtocol, existing: Option[Annotation]): Annotation = existing match {
    case Some(annotation) => Annotation(input.student, input.labwork, input.reportCardEntry, input.message, annotation.timestamp, annotation.id)
    case None => Annotation(input.student, input.labwork, input.reportCardEntry, input.message)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, annotation.create)
    case Update => SecureBlock(restrictionId, annotation.update)
    case Get => SecureBlock(restrictionId, annotation.get)
    case GetAll => SecureBlock(restrictionId, annotation.getAll)
    case Delete => SecureBlock(restrictionId, annotation.delete)
  }

  def createFrom(course: String, labwork: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    create(NonSecureBlock)(rebase(Annotation.generateBase))
  }

  def createAtomicFrom(course: String, labwork: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    createAtomic(NonSecureBlock)(rebase(Annotation.generateBase))
  }

  def updateFrom(course: String, labwork: String, annotation: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    update(annotation, NonSecureBlock)(rebase(Annotation.generateBase(UUID.fromString(annotation))))
  }

  def updateAtomicFrom(course: String, labwork: String, annotation: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    updateAtomic(annotation, NonSecureBlock)(rebase(Annotation.generateBase(UUID.fromString(annotation))))
  }

  def allFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    all(NonSecureBlock)(rebase(Annotation.generateBase, labworkAttribute -> Seq(labwork)))
  }

  def allAtomicFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    allAtomic(NonSecureBlock)(rebase(Annotation.generateBase, labworkAttribute -> Seq(labwork)))
  }

  def getFrom(course: String, labwork: String, annotation: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    get(annotation, NonSecureBlock)(rebase(Annotation.generateBase(UUID.fromString(annotation))))
  }

  def getAtomicFrom(course: String, labwork: String, annotation: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    getAtomic(annotation, NonSecureBlock)(rebase(Annotation.generateBase(UUID.fromString(annotation))))
  }

  def deleteFrom(course: String, labwork: String, annotation: String) = restrictedContext(course)(Delete) asyncAction { implicit request =>
    delete(annotation, NonSecureBlock)(rebase(Annotation.generateBase(UUID.fromString(annotation))))
  }
}