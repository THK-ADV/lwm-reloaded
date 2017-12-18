package controllers

import models.Permissions.{blacklist, prime}
import models._
import org.joda.time.Interval
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import services.{BlacklistServiceLike, RoleService, RoleServiceLike, SessionHandlingService}
import store.bind.Descriptor.Descriptor
import store.{Namespace, SesameRepository}
import utils.{LwmDateTime, LwmMimeType}
import controllers.BlacklistCRUDController._

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Try}

object BlacklistCRUDController {
  val selectAttribute = "select"
  val currentValue = "current"
}

class BlacklistCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleServiceLike, val blacklistService: BlacklistServiceLike) extends AbstractCRUDController[SesameBlacklistProtocol, SesameBlacklist, SesameBlacklist] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.blacklistV1Json

  override implicit val descriptor: Descriptor[Sesame, SesameBlacklist] = defaultBindings.BlacklistDescriptor

  override val descriptorAtom: Descriptor[Sesame, SesameBlacklist] = descriptor

  override implicit val reads: Reads[SesameBlacklistProtocol] = SesameBlacklist.reads

  override implicit val writes: Writes[SesameBlacklist] = SesameBlacklist.writes

  override implicit val writesAtom: Writes[SesameBlacklist] = SesameBlacklist.writesAtom

  override implicit val uriGenerator: UriGenerator[SesameBlacklist] = SesameBlacklist

  override protected def coAtomic(atom: SesameBlacklist): SesameBlacklist = atom

  override protected def compareModel(input: SesameBlacklistProtocol, output: SesameBlacklist): Boolean = {
    input.label == output.label && LwmDateTime.isEqual(input.dates, output.dates)
  }

  override protected def fromInput(input: SesameBlacklistProtocol, existing: Option[SesameBlacklist]): SesameBlacklist = existing match {
    case Some(blacklist) => SesameBlacklist(input.label, input.dates.map(LwmDateTime.toDateTime), blacklist.invalidated, blacklist.id)
    case None => SesameBlacklist(input.label, input.dates.map(LwmDateTime.toDateTime), None, SesameBlacklist.randomUUID)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(blacklist.get)
    case _ => PartialSecureBlock(prime)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[SesameBlacklist]): Try[Set[SesameBlacklist]] = {
    import defaultBindings.SemesterDescriptor
    import models.SesameSemester.isCurrent

    queryString.foldLeft(Try(all)) {
      case (set, (`selectAttribute`, current)) if current.head == currentValue =>
        for {
          semesters <- repository.getAll[SesameSemester]
          currentSemester = semesters.find(isCurrent)
          blacklists <- set
        } yield currentSemester.fold(Set.empty[SesameBlacklist]) { semester =>
          blacklists.map { blacklist =>
            blacklist.copy(blacklist.label, blacklist.dates.filter(date => new Interval(semester.start.toDateTimeAtCurrentTime, semester.end.toDateTimeAtCurrentTime).contains(date)))
          }.filter(_.dates.nonEmpty)
        }
      case (_, (`selectAttribute`, other)) => Failure(new Throwable(s"Value of $selectAttribute should be $currentValue, but was ${other.head}"))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  def createFor(year: String) = contextFrom(Create) asyncContentTypedAction { implicit request =>
    import scala.concurrent.ExecutionContext.Implicits.global

    (for { // TODO refactor to attempt
      blacklist <- blacklistService.fetchByYear(year)
      _ <- Future.fromTry(repository.add[SesameBlacklist](blacklist))
    } yield Created(Json.toJson(blacklist)(writes)).as(mimeType)) recover {
      case NonFatal(t) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> t.getMessage
      ))
    }
  }
}
