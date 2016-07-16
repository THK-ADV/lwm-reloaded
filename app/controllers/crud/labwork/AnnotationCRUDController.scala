package controllers.crud.labwork

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.labwork._
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import models.security.Permissions._
import scala.collection.Map
import scala.util.{Failure, Try}
import utils.RequestOps._
import AnnotationCRUDController._
import store.bind.Descriptor.Descriptor

object AnnotationCRUDController {
  val labworkAttribute = "labwork"
  val studentAttribute = "student"
  val reportCardEntryAttribute = "reportCardEntry"
}

class AnnotationCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[AnnotationProtocol, Annotation, AnnotationAtom] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.annotationV1Json

  override implicit val descriptor: Descriptor[Sesame, Annotation] = defaultBindings.AnnotationDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, AnnotationAtom] = defaultBindings.AnnotationAtomDescriptor

  override implicit val reads: Reads[AnnotationProtocol] = Annotation.reads

  override implicit val writes: Writes[Annotation] = Annotation.writes

  override implicit val writesAtom: Writes[AnnotationAtom] = Annotation.writesAtom

  override implicit val uriGenerator: UriGenerator[Annotation] = Annotation

  override protected def coatomic(atom: AnnotationAtom): Annotation = Annotation(atom.student.id, atom.labwork.id, atom.reportCardEntry.id, atom.message, atom.timestamp, atom.invalidated, atom.id)

  override protected def compareModel(input: AnnotationProtocol, output: Annotation): Boolean = input.message == output.message

  override protected def fromInput(input: AnnotationProtocol, existing: Option[Annotation]): Annotation = existing match {
    case Some(annotation) => Annotation(input.student, input.labwork, input.reportCardEntry, input.message, annotation.timestamp, annotation.invalidated, annotation.id)
    case None => Annotation(input.student, input.labwork, input.reportCardEntry, input.message)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, annotation.create)
    case Update => SecureBlock(restrictionId, annotation.update)
    case Get => SecureBlock(restrictionId, annotation.get)
    case GetAll => SecureBlock(restrictionId, annotation.getAll)
    case Delete => SecureBlock(restrictionId, annotation.delete)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Annotation]): Try[Set[Annotation]] = {
    queryString.foldRight(Try[Set[Annotation]](all)) {
      case ((`labworkAttribute`, values), t) => t flatMap (set => Try(UUID.fromString(values.head)).map(id => set.filter(_.labwork == id)))
      case ((`studentAttribute`, values), t) => t flatMap (set => Try(UUID.fromString(values.head)).map(id => set.filter(_.student == id)))
      case ((`reportCardEntryAttribute`, values), t) => t flatMap (set => Try(UUID.fromString(values.head)).map(id => set.filter(_.reportCardEntry == id)))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
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