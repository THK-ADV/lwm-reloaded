package controllers.crud

import java.util.UUID

import models.security.Permission
import models.{Labwork, LabworkProtocol, UriGenerator}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import scala.collection.Map
import scala.util.{Try, Failure, Success}
import LabworkCRUDController._

object LabworkCRUDController {
  val courseParameter = "course"
  val degreeParameter = "degree"
  val semesterParamter = "semester"

}

class LabworkCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[LabworkProtocol, Labwork] {
  override implicit def rdfWrites: ToPG[Sesame, Labwork] = defaultBindings.LabworkBinding.labworkBinder

  override implicit def rdfReads: FromPG[Sesame, Labwork] = defaultBindings.LabworkBinding.labworkBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Labwork] = defaultBindings.LabworkBinding.classUri

  override implicit def uriGenerator: UriGenerator[Labwork] = Labwork

  override implicit def reads: Reads[LabworkProtocol] = Labwork.reads

  override implicit def writes: Writes[Labwork] = Labwork.writes

  override protected def fromInput(input: LabworkProtocol, id: Option[UUID]): Labwork = id match {
    case Some(i) => Labwork(input.label, input.description, input.semester, input.course, input.degree, input.assignmentPlan, i)
    case None => Labwork(input.label, input.description, input.semester, input.course, input.degree, input.assignmentPlan, Labwork.randomUUID)
  }

  override val mimeType: LwmMimeType = LwmMimeType.labworkV1Json

  override def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Labwork]): Result = {
    val filtered = queryString.foldRight(Try[Set[Labwork]](all)) {
      case ((`courseParameter`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.course == p)))
      case ((`degreeParameter`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.degree == p)))
      case ((`semesterParamter`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.semester == p)))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }

    filtered match {
      case Success(f) =>
        if (f.isEmpty) {
          NotFound(Json.obj(
            "status" -> "KO",
            "message" -> "No such element..."
          ))
        } else
          Ok(Json.toJson(f))

      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }
}
