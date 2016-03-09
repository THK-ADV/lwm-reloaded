package controllers.crud

import java.util.UUID

import models._
import models.semester.Semester
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, JsValue, Reads, Writes}
import services.RoleService
import store.Prefixes.LWMPrefix
import store.sparql.select._
import store.sparql.{select, Clause}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import scala.collection.Map
import scala.util.{Try, Failure}
import LabworkCRUDController._
import models.security.Permissions._

object LabworkCRUDController {
  val courseAttribute = "course"
  val degreeAttribute = "degree"
  val semesterAttribute = "semester"
}

class LabworkCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[LabworkProtocol, Labwork] {

  override implicit def rdfWrites: ToPG[Sesame, Labwork] = defaultBindings.LabworkBinding.labworkBinder

  override implicit def rdfReads: FromPG[Sesame, Labwork] = defaultBindings.LabworkBinding.labworkBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Labwork] = defaultBindings.LabworkBinding.classUri

  override implicit def uriGenerator: UriGenerator[Labwork] = Labwork

  override implicit def reads: Reads[LabworkProtocol] = Labwork.reads

  override implicit def writes: Writes[Labwork] = Labwork.writes

  override protected def fromInput(input: LabworkProtocol, id: Option[UUID]): Labwork = id match {
    case Some(uuid) => Labwork(input.label, input.description, input.semester, input.course, input.degree, input.assignmentPlan, uuid)
    case None => Labwork(input.label, input.description, input.semester, input.course, input.degree, input.assignmentPlan, Labwork.randomUUID)
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
    input.label == output.label && input.description == output.description && input.assignmentPlan.entries == output.assignmentPlan.entries
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
        val atom = LabworkAtom(output.label, output.description, s, c, d, output.assignmentPlan, output.id)
        Json.toJson(atom)
      }
    }
  }

  override protected def atomizeMany(output: Set[Labwork]): Try[JsValue] = {
    import defaultBindings.SemesterBinding.semesterBinder
    import defaultBindings.DegreeBinding.degreeBinder
    import defaultBindings.CourseBinding.courseBinder
    import Labwork.atomicWrites

    (for {
      semesters <- repository.getMany[Semester](output.map(l => Semester.generateUri(l.semester)(namespace)))
      courses <- repository.getMany[Course](output.map(l => Course.generateUri(l.course)(namespace)))
      degrees <- repository.getMany[Degree](output.map(l => Degree.generateUri(l.degree)(namespace)))
    } yield {
      output.foldLeft(Set.empty[LabworkAtom]) { (newSet, labwork) =>
        (for {
          s <- semesters.find(_.id == labwork.semester)
          c <- courses.find(_.id == labwork.course)
          d <- degrees.find(_.id == labwork.degree)
        } yield LabworkAtom(labwork.label, labwork.description, s, c, d, labwork.assignmentPlan, labwork.id)) match {
          case Some(atom) => newSet + atom
          case None => newSet
        }
      }
    }).map(s => Json.toJson(s))
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

  def createFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { request =>
    super.create(NonSecureBlock)(request)
  }

  def createAtomicFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { request =>
    super.createAtomic(NonSecureBlock)(request)
  }

  def updateFrom(course: String, labwork: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, Labwork.generateBase(UUID.fromString(labwork)))
    super.update(labwork, NonSecureBlock)(newRequest)
  }

  def deleteFrom(course: String, labwork: String) = restrictedContext(course)(Delete) asyncAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, Labwork.generateBase(UUID.fromString(labwork)))
    super.delete(labwork, NonSecureBlock)(newRequest)
  }

  def getFrom(course: String, labwork: String) = restrictedContext(course)(Get) asyncAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, Labwork.generateBase(UUID.fromString(labwork)))
    super.get(labwork, NonSecureBlock)(newRequest)
  }

  def getAtomicFrom(course: String, labwork: String) = restrictedContext(course)(Get) asyncAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, Labwork.generateBase(UUID.fromString(labwork)))
    super.getAtomic(labwork, NonSecureBlock)(newRequest)
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    super.all(NonSecureBlock)(request)
  }

  def allAtomicFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    super.allAtomic(NonSecureBlock)(request)
  }
}
