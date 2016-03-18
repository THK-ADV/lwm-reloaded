package controllers.crud

import java.util.UUID

import models._
import models.semester.Semester
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.sparql.select._
import store.sparql.{Clause, select}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import utils.RequestOps._
import LabworkCRUDController._
import scala.collection.Map
import scala.util.{Failure, Try}
import models.security.Permissions._

object LabworkCRUDController {
  val courseAttribute = "course"
  val degreeAttribute = "degree"
  val semesterAttribute = "semester"
}

class LabworkCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[LabworkProtocol, Labwork] {

  override implicit def rdfWrites: ToPG[Sesame, Labwork] = defaultBindings.LabworkBinding.labworkBinder

  override implicit def rdfReads: FromPG[Sesame, Labwork] = defaultBindings.LabworkBinding.labworkBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Labwork] = defaultBindings.LabworkBinding.classUri

  override implicit def uriGenerator: UriGenerator[Labwork] = Labwork

  override implicit def reads: Reads[LabworkProtocol] = Labwork.reads

  override implicit def writes: Writes[Labwork] = Labwork.writes

  override protected def fromInput(input: LabworkProtocol, existing: Option[Labwork]): Labwork = existing match {
    case Some(labwork) => Labwork(input.label, input.description, input.semester, input.course, input.degree, labwork.id)
    case None => Labwork(input.label, input.description, input.semester, input.course, input.degree, Labwork.randomUUID)
  }

  override val mimeType: LwmMimeType = LwmMimeType.labworkV1Json

  override protected def existsQuery(input: LabworkProtocol): (Clause, select.Var) = {
    lazy val prefixes = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    (select ("id") where {
      ^(v("s"), p(rdf.`type`), s(prefixes.Labwork)) .
        ^(v("s"), p(prefixes.semester), s(Semester.generateUri(input.semester)(namespace))) .
        ^(v("s"), p(prefixes.course), s(Course.generateUri(input.course)(namespace))) .
        ^(v("s"), p(prefixes.degree), s(Degree.generateUri(input.degree)(namespace))) .
        ^(v("s"), p(prefixes.id), v("id"))
    }, v("id"))
  }

  override protected def compareModel(input: LabworkProtocol, output: Labwork): Boolean = {
    input.label == output.label && input.description == output.description
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Labwork]): Try[Set[Labwork]] = {
    queryString.foldRight(Try[Set[Labwork]](all)) {
      case ((`courseAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.course == p)))
      case ((`degreeAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.degree == p)))
      case ((`semesterAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.semester == p)))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def atomize(output: Labwork): Try[Option[JsValue]] = {
    import defaultBindings.SemesterBinding.semesterBinder
    import defaultBindings.DegreeBinding.degreeBinder
    import defaultBindings.CourseBinding.courseBinder
    import Labwork.atomicWrites

    for {
      semester <- repository.get[Semester](Semester.generateUri(output.semester)(namespace))
      course <- repository.get[Course](Course.generateUri(output.course)(namespace))
      degree <- repository.get[Degree](Degree.generateUri(output.degree)(namespace))
    } yield {
      for {
        s <- semester
        c <- course
        d <- degree
      } yield {
        val atom = LabworkAtom(output.label, output.description, s, c, d, output.id)
        Json.toJson(atom)
      }
    }
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(labwork.get)
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
    allAtomic(NonSecureBlock)(rebase(Labwork.generateBase))
  }
}
