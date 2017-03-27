package controllers

import java.util.UUID

import models.{DegreeDb, DegreeProtocol}
import models.Permissions.{degree, god, prime}
import play.api.mvc.Controller
import services._
import store.{DegreeTable, TableFilter}
import utils.LwmMimeType

import scala.concurrent.Future
import scala.util.{Failure, Try}

object DegreeCRUDControllerPostgres {
  lazy val labelAttribute = "label"
  lazy val abbreviationAttribute = "abbreviation"
}

final class DegreeCRUDControllerPostgres(val degreeService: DegreeService, val sessionService: SessionHandlingService, val roleService: RoleService) extends Controller
  with Secured
  with SessionChecking
  with SecureControllerContext
  with ContentTyped
  with Chunked
  with PostgresResult {

  override implicit def mimeType: LwmMimeType = LwmMimeType.degreeV1Json
  import scala.concurrent.ExecutionContext.Implicits.global
  import models.PostgresDegree.{writes, reads}

  def all = contextFrom(GetAll) asyncAction{ request =>
    import controllers.DegreeCRUDControllerPostgres._

    val degreeFilter = request.queryString.foldLeft(Try(List.empty[TableFilter[DegreeTable]])) {
      case (list, (`labelAttribute`, label)) => list.map(_.+:(DegreeLabelFilter(label.head)))
      case (list, (`abbreviationAttribute`, abbreviation)) => list.map(_.+:(DegreeAbbreviationFilter(abbreviation.head)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }

    (for{
      list <- Future.fromTry(degreeFilter)
      degrees <- degreeService.get(list)
    } yield degrees).jsonResult
  }

  def get(id: String) = contextFrom(Get) asyncAction { _ =>
    degreeService.get(List(DegreeIdFilter(id))).map(_.headOption).jsonResult(id)
  }

  def update(id: String) = contextFrom(Update) asyncContentTypedAction { request =>
    val uuid = UUID.fromString(id)

    (for {
      degreeProtocol <- Future.fromTry(parse[DegreeProtocol](request))
      degreeDb = DegreeDb(degreeProtocol.label, degreeProtocol.abbreviation, None, uuid)
      result <- degreeService.update(degreeDb)
    } yield result.map(_.toDegree)).jsonResult(uuid)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(degree.get)
    case GetAll => PartialSecureBlock(degree.getAll)
    case Update => PartialSecureBlock(prime)
    case _ => PartialSecureBlock(god)
  }
}
