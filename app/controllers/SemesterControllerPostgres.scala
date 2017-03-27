package controllers

import java.util.UUID

import models.Permissions.{prime, semester}
import models.{SemesterDb, SemesterProtocol}
import play.api.mvc.Controller
import services._
import utils.LwmMimeType

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object SemesterControllerPostgres {
  val selectAttribute = "select"
  val currentValue = "current"
}

final class SemesterControllerPostgres(val sessionService: SessionHandlingService, val roleService: RoleService, val semesterService: SemesterService) extends Controller
  with Secured
  with SessionChecking
  with SecureControllerContext
  with ContentTyped
  with Chunked
  with PostgresResult {

  import models.PostgresSemester.{writes, reads}
  import scala.concurrent.ExecutionContext.Implicits.global

  override implicit def mimeType = LwmMimeType.semesterV1Json

  def create = contextFrom(Create) asyncContentTypedAction { request =>
    (for {
      protocol <- Future.fromTry(parse[SemesterProtocol](request))
      semesterDb = SemesterDb(protocol.label, protocol.abbreviation, protocol.start, protocol.end, protocol.examStart)
      semester <- semesterService.create(semesterDb)
    } yield semester.toSemester).jsonResult
  }

  def update(id: String) = contextFrom(Update) asyncContentTypedAction { request =>
    val uuid = UUID.fromString(id)

    (for {
      protocol <- Future.fromTry(parse[SemesterProtocol](request))
      semesterDb = SemesterDb(protocol.label, protocol.abbreviation, protocol.start, protocol.end, protocol.examStart, None, uuid)
      semester <- semesterService.update(semesterDb)
    } yield semester.map(_.toSemester)).jsonResult(uuid)
  }

  def delete(id: String) = contextFrom(Delete) asyncAction { _ =>
    val uuid = UUID.fromString(id)

    semesterService.delete(uuid).map(_.map(_.toSemester)).jsonResult(uuid)
  }

  def all = contextFrom(GetAll) asyncAction { request =>
    import controllers.SemesterControllerPostgres._
    import models.PostgresSemester.isCurrent

    (for{
      semesters <- semesterService.get()
      filter = request.queryString.get(`selectAttribute`).fold[Try[Boolean]](Success(false)) { seq =>
        if (seq.headOption.contains(currentValue))
          Success(true)
        else
          Failure(new Throwable(s"Value of $selectAttribute should be $currentValue, but was ${seq.headOption}"))
      }
      currentFilter <- Future.fromTry(filter)
    } yield if (currentFilter) semesters.filter(isCurrent) else semesters).jsonResult
  }

  def get(id: String) = contextFrom(Get) asyncAction { _ =>
    semesterService.get(List(SemesterIdFilter(id))).map(_.headOption).jsonResult(id)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(semester.get)
    case GetAll => PartialSecureBlock(semester.getAll)
    case _ => PartialSecureBlock(prime)
  }
}
