package controllers.crud.semester

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.security.Permissions._
import models.semester.{Semester, SemesterProtocol}
import org.joda.time.Interval
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.sparql.{Clause, select}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Success, Try}

class SemesterCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[SemesterProtocol, Semester, Semester] {
  override val mimeType: LwmMimeType = LwmMimeType.semesterV1Json

  override implicit def descriptor: Descriptor[Sesame, Semester] = defaultBindings.SemesterDescriptor

  override implicit def uriGenerator: UriGenerator[Semester] = Semester

  override implicit def reads: Reads[SemesterProtocol] = Semester.reads

  override implicit def writes: Writes[Semester] = Semester.writes

  override implicit def writesAtom: Writes[Semester] = Semester.writesAtom

  override protected def fromInput(input: SemesterProtocol, existing: Option[Semester]): Semester = existing match {
    case Some(semester) => Semester(input.label, input.abbreviation, input.start, input.end, input.examStart, semester.invalidated, semester.id)
    case None => Semester(input.label, input.abbreviation, input.start, input.end, input.examStart, None, Semester.randomUUID)
  }

  override protected def existsQuery(input: SemesterProtocol): (Clause, select.Var) = {
    import store.sparql.select
    import store.sparql.select._

    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    (select ("id") where {
      **(v("s"), p(rdf.`type`), s(lwm.Semester)) .
        **(v("s"), p(lwm.label), o(input.label)) .
        **(v("s"), p(lwm.start), o(input.start)) .
        **(v("s"), p(lwm.end), o(input.end)) .
        **(v("s"), p(lwm.id), v("id"))
    }, v("id"))
  }

  override protected def coAtomic(atom: Semester): Semester = atom

  override implicit def descriptorAtom: Descriptor[Sesame, Semester] = descriptor

  override protected def compareModel(input: SemesterProtocol, output: Semester): Boolean = {
    input.abbreviation == output.abbreviation && input.examStart.isEqual(output.examStart)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(semester.get)
    case GetAll => PartialSecureBlock(semester.getAll)
    case _ => PartialSecureBlock(prime)
  }

  def current = contextFrom(Get) action { implicit request =>
    import models.semester.Semester.writes
    import models.semester.Semester.findCurrent

    retrieveAll[Semester](descriptor).
      flatMap(semesters => optional2(findCurrent(semesters))).
      mapResult(semester => Ok(Json.toJson(semester)).as(mimeType))
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Semester]): Try[Set[Semester]] = Success(all)
}