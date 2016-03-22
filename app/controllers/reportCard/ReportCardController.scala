package controllers.reportCard

import java.util.UUID

import controllers.crud._
import models._
import models.security.Permissions._
import models.users.{Student, User}
import modules.store.BaseNamespace
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc._
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.util.{Failure, Success, Try}

class ReportCardController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends Controller
  with BaseNamespace
  with JsonSerialisation[ReportCard, ReportCard]
  with SesameRdfSerialisation[ReportCard]
  with Atomic[ReportCard]
  with ContentTyped
  with Secured
  with SessionChecking
  with SecureControllerContext {

  override implicit def rdfReads: FromPG[Sesame, ReportCard] = defaultBindings.ReportCardBinding.reportCardBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, ReportCard] = defaultBindings.ReportCardBinding.classUri

  override implicit def uriGenerator: UriGenerator[ReportCard] = ReportCard

  override implicit def rdfWrites: ToPG[Sesame, ReportCard] = defaultBindings.ReportCardBinding.reportCardBinder

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardV1Json

  override implicit def reads: Reads[ReportCard] = ReportCard.reads

  override implicit def writes: Writes[ReportCard] = ReportCard.writes

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(reportCard.get)
    case _ => PartialSecureBlock(god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(god)
  }

  def get(student: String) = reportCardFor(student) { card =>
    Success(Some(Ok(Json.toJson(card)).as(mimeType)))
  }

  def getAtomic(student: String) = reportCardFor(student) { card =>
    import utils.Ops.MonadInstances.{optM, tryM}
    import utils.Ops._

    atomize(card).peek(json => Ok(json).as(mimeType))
  }

  private def reportCardFor(student: String)(f: ReportCard => Try[Option[Result]]) = contextFrom(Get) action { request =>
    import store.sparql.select
    import store.sparql.select._
    import utils.Ops.MonadInstances.{optM, tryM}
    import utils.Ops.NaturalTrasformations._
    import utils.Ops.TraverseInstances.{travO, travT}
    import utils.Ops._

    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    val query = select ("card") where {
      ^(v("card"), p(rdf.`type`), s(lwm.ReportCard)).
      ^(v("card"), p(lwm.student), s(User.generateUri(UUID.fromString(student))(namespace)))
    }

    val result = repository.prepareQuery(query).
      select(_.get("card")).
      changeTo(_.headOption).
      map(_.stringValue)(optM).
      request[Option, ReportCard](repository.get[ReportCard]).
      run

    result.flatPeek(f) match {
      case Success(Some(res)) => res
      case Success(None) =>
        NotFound(Json.obj(
          "status" -> "KO",
          "message" -> "No such element..."
        ))
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  override protected def atomize(output: ReportCard): Try[Option[JsValue]] = {
    import ReportCard.atomicWrites
    import defaultBindings.LabworkBinding.labworkBinder
    import defaultBindings.RoomBinding.roomBinder
    import defaultBindings.StudentBinding.studentBinder

    for {
      student <- repository.get[Student](User.generateUri(output.student)(namespace))
      labwork <- repository.get[Labwork](Labwork.generateUri(output.labwork)(namespace))
      roomIds = output.entries.map(e => Room.generateUri(e.room)(namespace))
      rooms <- repository.getMany[Room](roomIds)
    } yield {
      for {
        s <- student; l <- labwork
      } yield {
        val entries = output.entries.foldLeft(Set.empty[ReportCardEntryAtom]) { (newSet, e) =>
          rooms.find(_.id == e.room) match {
            case Some(r) => newSet + ReportCardEntryAtom(e.index, e.label, e.date, e.start, e.end, r, e.entryTypes, e.id)
            case None => newSet
          }
        }

        Json.toJson(ReportCardAtom(s, l, entries, output.id))(atomicWrites)
      }
    }
  }
}
