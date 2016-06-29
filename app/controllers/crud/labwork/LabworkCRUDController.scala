package controllers.crud.labwork

import java.util.UUID

import controllers.crud.AbstractCRUDController
import controllers.crud.labwork.LabworkCRUDController._
import models.{Course, CourseAtom, Degree, UriGenerator}
import models.labwork.{Labwork, LabworkAtom, LabworkProtocol}
import models.security.Permissions._
import models.semester.Semester
import models.users.{Employee, User}
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.{CompositeClassUris, Descriptor}
import store.sparql.select._
import store.sparql.{Clause, select}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import utils.RequestOps._

import scala.collection.Map
import scala.util.{Failure, Try}

object LabworkCRUDController {
  val courseAttribute = "course"
  val degreeAttribute = "degree"
  val semesterAttribute = "semester"
  val subscribableAttribute = "subscribable"
  val publishedAttribute = "published"
}

class LabworkCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[LabworkProtocol, Labwork, LabworkAtom] {

  override implicit def descriptor: Descriptor[Sesame, Labwork] = defaultBindings.LabworkDescriptor

  override implicit def descriptorAtom: Descriptor[Sesame, LabworkAtom] = defaultBindings.LabworkAtomDescriptor

  override implicit def uriGenerator: UriGenerator[Labwork] = Labwork

  override implicit def reads: Reads[LabworkProtocol] = Labwork.reads

  override implicit def writes: Writes[Labwork] = Labwork.writes

  override implicit def writesAtom: Writes[LabworkAtom] = Labwork.writesAtom

  override protected def fromInput(input: LabworkProtocol, existing: Option[Labwork]): Labwork = existing match {
    case Some(labwork) =>
      Labwork(input.label, input.description, input.semester, input.course, input.degree, input.subscribable, input.published, labwork.id)
    case None =>
      Labwork(input.label, input.description, input.semester, input.course, input.degree, input.subscribable, input.published, Labwork.randomUUID)
  }

  override val mimeType: LwmMimeType = LwmMimeType.labworkV1Json

  override protected def existsQuery(input: LabworkProtocol): (Clause, select.Var) = {
    lazy val prefixes = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    (select("id") where {
      **(v("s"), p(rdf.`type`), s(prefixes.Labwork)).
        **(v("s"), p(prefixes.semester), s(Semester.generateUri(input.semester)(namespace))).
        **(v("s"), p(prefixes.course), s(Course.generateUri(input.course)(namespace))).
        **(v("s"), p(prefixes.degree), s(Degree.generateUri(input.degree)(namespace))).
        **(v("s"), p(prefixes.id), v("id"))
    }, v("id"))
  }

  override protected def compareModel(input: LabworkProtocol, output: Labwork): Boolean = {
    input.label == output.label && input.description == output.description && input.subscribable == output.subscribable && input.published == output.published
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Labwork]): Try[Set[Labwork]] = {
    queryString.foldRight(Try[Set[Labwork]](all)) {
      case ((`courseAttribute`, values), set) => set flatMap (set => Try(UUID.fromString(values.head)).map(p => set.filter(_.course == p)))
      case ((`degreeAttribute`, values), set) => set flatMap (set => Try(UUID.fromString(values.head)).map(p => set.filter(_.degree == p)))
      case ((`semesterAttribute`, values), set) => set flatMap (set => Try(UUID.fromString(values.head)).map(p => set.filter(_.semester == p)))
      case ((`subscribableAttribute`, values), set) => set flatMap (set => Try(values.head.toBoolean).map(b => set.filter(_.subscribable == b)))
      case ((`publishedAttribute`, values), set) => set flatMap (set => Try(values.head.toBoolean).map(b => set.filter(_.published == b)))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(labwork.get)
    case GetAll => PartialSecureBlock(labwork.getAll)
    case _ => PartialSecureBlock(god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, labwork.create)
    case GetAll => SecureBlock(restrictionId, labwork.getAll)
    case Update => SecureBlock(restrictionId, labwork.update)
    case Get => SecureBlock(restrictionId, labwork.get)
    case Delete => PartialSecureBlock(prime)
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    create(NonSecureBlock)(rebase(Labwork.generateBase))
  }

  def createAtomicFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    createAtomic(NonSecureBlock)(rebase(Labwork.generateBase))
  }

  def updateFrom(course: String, labwork: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    update(labwork, NonSecureBlock)(rebase(Labwork.generateBase(UUID.fromString(labwork))))
  }

  def updateAtomicFrom(course: String, labwork: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    updateAtomic(labwork, NonSecureBlock)(rebase(Labwork.generateBase(UUID.fromString(labwork))))
  }

  def deleteFrom(course: String, labwork: String) = restrictedContext(course)(Delete) asyncAction { implicit request =>
    delete(labwork, NonSecureBlock)(rebase(Labwork.generateBase(UUID.fromString(labwork))))
  }

  def getFrom(course: String, labwork: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    get(labwork, NonSecureBlock)(rebase(Labwork.generateBase(UUID.fromString(labwork))))
  }

  def getAtomicFrom(course: String, labwork: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    getAtomic(labwork, NonSecureBlock)(rebase(Labwork.generateBase(UUID.fromString(labwork))))
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    all(NonSecureBlock)(rebase(Labwork.generateBase, courseAttribute -> Seq(course)))
  }

  def allAtomicFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    allAtomic(NonSecureBlock)(rebase(Labwork.generateBase, courseAttribute -> Seq(course)))
  }

  def allWithDegree(degree: String) = contextFrom(GetAll) asyncAction { implicit request =>
    all(NonSecureBlock)(rebase(Labwork.generateBase, degreeAttribute -> Seq(degree), subscribableAttribute -> Seq(true.toString)))
  }

  def allAtomicWithDegree(degree: String) = contextFrom(GetAll) asyncAction { implicit request =>
    allAtomic(NonSecureBlock)(rebase(Labwork.generateBase, degreeAttribute -> Seq(degree), subscribableAttribute -> Seq(true.toString)))
  }

  override protected def coatomic(atom: LabworkAtom): Labwork =
    Labwork(
      atom.label,
      atom.description,
      atom.semester.id,
      atom.course.id,
      atom.degree.id,
      atom.subscribable,
      atom.published,
      atom.id)
}