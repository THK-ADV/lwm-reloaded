package controllers

import java.util.UUID

import models.Permissions.{god, labwork, prime}
import models._
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleService, RoleServiceLike, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.sparql.select._
import store.sparql.{Clause, select}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import controllers.LabworkCRUDController._

import scala.collection.Map
import scala.util.{Failure, Try}

object LabworkCRUDController {
  val courseAttribute = "course"
  val degreeAttribute = "degree"
  val semesterAttribute = "semester"
  val subscribableAttribute = "subscribable"
  val publishedAttribute = "published"
}

class LabworkCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleServiceLike) extends AbstractCRUDController[LabworkProtocol, SesameLabwork, SesameLabworkAtom] {

  override val mimeType: LwmMimeType = LwmMimeType.labworkV1Json

  override implicit val descriptor: Descriptor[Sesame, SesameLabwork] = defaultBindings.LabworkDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, SesameLabworkAtom] = defaultBindings.LabworkAtomDescriptor

  override implicit val reads: Reads[LabworkProtocol] = SesameLabwork.reads

  override implicit val writes: Writes[SesameLabwork] = SesameLabwork.writes

  override implicit val writesAtom: Writes[SesameLabworkAtom] = SesameLabwork.writesAtom

  override implicit val uriGenerator: UriGenerator[SesameLabwork] = SesameLabwork

  override protected def coAtomic(atom: SesameLabworkAtom): SesameLabwork = SesameLabwork(
    atom.label,
    atom.description,
    atom.semester.id,
    atom.course.id,
    atom.degree.id,
    atom.subscribable,
    atom.published,
    atom.invalidated,
    atom.id
  )

  override protected def compareModel(input: LabworkProtocol, output: SesameLabwork): Boolean = {
    input.label == output.label && input.description == output.description && input.subscribable == output.subscribable && input.published == output.published
  }

  override protected def fromInput(input: LabworkProtocol, existing: Option[SesameLabwork]): SesameLabwork = existing match {
    case Some(labwork) =>
      SesameLabwork(input.label, input.description, input.semester, input.course, input.degree, input.subscribable, input.published, labwork.invalidated, labwork.id)
    case None =>
      SesameLabwork(input.label, input.description, input.semester, input.course, input.degree, input.subscribable, input.published)
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

  override protected def existsQuery(input: LabworkProtocol): Clause = {
    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    select("s") where {
      **(v("s"), p(rdf.`type`), s(lwm.Labwork)).
        **(v("s"), p(lwm.semester), s(SesameSemester.generateUri(input.semester)(namespace))).
        **(v("s"), p(lwm.course), s(SesameCourse.generateUri(input.course)(namespace))).
        **(v("s"), p(lwm.degree), s(SesameDegree.generateUri(input.degree)(namespace)))
    }
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[SesameLabwork]): Try[Set[SesameLabwork]] = {
    queryString.foldRight(Try[Set[SesameLabwork]](all)) {
      case ((`courseAttribute`, values), set) => set flatMap (set => Try(UUID.fromString(values.head)).map(p => set.filter(_.course == p)))
      case ((`degreeAttribute`, values), set) => set flatMap (set => Try(UUID.fromString(values.head)).map(p => set.filter(_.degree == p)))
      case ((`semesterAttribute`, values), set) => set flatMap (set => Try(UUID.fromString(values.head)).map(p => set.filter(_.semester == p)))
      case ((`subscribableAttribute`, values), set) => set flatMap (set => Try(values.head.toBoolean).map(b => set.filter(_.subscribable == b)))
      case ((`publishedAttribute`, values), set) => set flatMap (set => Try(values.head.toBoolean).map(b => set.filter(_.published == b)))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    create(NonSecureBlock)(rebase)
  }

  def createAtomicFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    createAtomic(NonSecureBlock)(rebase)
  }

  def updateFrom(course: String, labwork: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    update(labwork, NonSecureBlock)(rebase(labwork))
  }

  def updateAtomicFrom(course: String, labwork: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    updateAtomic(labwork, NonSecureBlock)(rebase(labwork))
  }

  def deleteFrom(course: String, labwork: String) = restrictedContext(course)(Delete) asyncAction { implicit request =>
    delete(labwork, NonSecureBlock)(rebase(labwork))
  }

  def getFrom(course: String, labwork: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    get(labwork, NonSecureBlock)(rebase(labwork))
  }

  def getAtomicFrom(course: String, labwork: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    getAtomic(labwork, NonSecureBlock)(rebase(labwork))
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    all(NonSecureBlock)(rebase(courseAttribute -> Seq(course)))
  }

  def allAtomicFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    allAtomic(NonSecureBlock)(rebase(courseAttribute -> Seq(course)))
  }

  def allWithDegree(degree: String) = contextFrom(GetAll) asyncAction { implicit request =>
    all(NonSecureBlock)(rebase(degreeAttribute -> Seq(degree), subscribableAttribute -> Seq(true.toString)))
  }

  def allAtomicWithDegree(degree: String) = contextFrom(GetAll) asyncAction { implicit request =>
    allAtomic(NonSecureBlock)(rebase(degreeAttribute -> Seq(degree), subscribableAttribute -> Seq(true.toString)))
  }
}