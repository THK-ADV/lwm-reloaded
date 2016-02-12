package controllers.crud

import java.net.URLDecoder
import java.util.UUID
import models.UriGenerator
import models.applications.{LabworkApplication, LabworkApplicationProtocol}
import org.joda.time.DateTime
import org.w3.banana.binder.{FromPG, ClassUrisFor, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import LabworkApplicationCRUDController._
import scala.collection.Map
import scala.util.{Try, Failure, Success}

object LabworkApplicationCRUDController {
  val labworkAttribute = "labwork"
  val applicantAttribute = "applicant"
  val friendAttribute = "friend"
  val dateAttribute = "date"
  val minTime = "minTime"
  val maxTime = "maxTime"
}

//DateTime format is: yyyy-MM-dd'T'HH:mm
class LabworkApplicationCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[LabworkApplicationProtocol, LabworkApplication] {

  override def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[LabworkApplication]): Result = {
    def decode(s: String) = URLDecoder.decode(s, "UTF-8")

    val filtered = queryString.foldRight(Try(all)) {
      case ((`labworkAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(v => set.filter(_.labwork == v)))
      case ((`applicantAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(v => set.filter(_.applicant == v)))
      case ((`friendAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(v => set.filter (_.friends.exists(_ == v))))
      case ((`dateAttribute`, v), set) => set map (_.filter (_.timestamp.toString("yyyy-MM-dd") == v.head))
      case ((`minTime`, v), t) => t flatMap (set => Try(DateTime.parse(decode(v.head))).map(t => set.filter(_.timestamp.isAfter(t))))
      case ((`maxTime`, v), t) => t flatMap (set => Try(DateTime.parse(decode(v.head))).map(t => set.filter(_.timestamp.isBefore(t))))
      case ((_, _), t) => Failure(new Throwable("Unknown attribute"))
    }

    filtered match {
      case Success(s) =>
        if (s.isEmpty)
          NotFound(Json.obj(
            "status" -> "KO",
            "message" -> "No such element..."
          ))
        else
          Ok(Json.toJson(s)).as(mimeType)

      case Failure(e) =>
        BadRequest(Json.obj(
          "status" -> "KO",
          "message" -> e.getMessage
        ))
    }
  }

  override implicit def reads: Reads[LabworkApplicationProtocol] = LabworkApplication.reads

  override implicit def writes: Writes[LabworkApplication] = LabworkApplication.writes

  override protected def fromInput(input: LabworkApplicationProtocol, id: Option[UUID]): LabworkApplication = id match {
    case Some(uuid) => LabworkApplication(input.labwork, input.applicant, input.friends, DateTime.now, uuid)
    case None => LabworkApplication(input.labwork, input.applicant, input.friends)
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.labworkApplicationV1Json

  override implicit def rdfReads: FromPG[Sesame, LabworkApplication] = defaultBindings.LabworkApplicationBinding.labworkApplicationBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, LabworkApplication] = defaultBindings.LabworkApplicationBinding.classUri

  override implicit def uriGenerator: UriGenerator[LabworkApplication] = LabworkApplication

  override implicit def rdfWrites: ToPG[Sesame, LabworkApplication] = defaultBindings.LabworkApplicationBinding.labworkApplicationBinder
}
