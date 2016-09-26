package controllers.crud.semester

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.security.Permissions.{semester, _}
import models.semester.{Blacklist, BlacklistProtocol, Semester}
import org.joda.time.Interval
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import services.{BlacklistServiceLike, RoleService, SessionHandlingService}
import store.bind.Descriptor.Descriptor
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object BlacklistCRUDController {
  val selectAttribute = "select"
  val currentValue = "current"
}

class BlacklistCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService, val blacklistService: BlacklistServiceLike) extends AbstractCRUDController[BlacklistProtocol, Blacklist, Blacklist] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.blacklistV1Json

  override implicit val descriptor: Descriptor[Sesame, Blacklist] = defaultBindings.BlacklistDescriptor

  override val descriptorAtom: Descriptor[Sesame, Blacklist] = descriptor

  override implicit val reads: Reads[BlacklistProtocol] = Blacklist.reads

  override implicit val writes: Writes[Blacklist] = Blacklist.writes

  override implicit val writesAtom: Writes[Blacklist] = Blacklist.writesAtom

  override implicit val uriGenerator: UriGenerator[Blacklist] = Blacklist

  override protected def coAtomic(atom: Blacklist): Blacklist = atom

  override protected def compareModel(input: BlacklistProtocol, output: Blacklist): Boolean = {
    input.label == output.label && input.dates == output.dates
  }

  override protected def fromInput(input: BlacklistProtocol, existing: Option[Blacklist]): Blacklist = existing match {
    case Some(blacklist) => Blacklist(input.label, input.dates, blacklist.invalidated, blacklist.id)
    case None => Blacklist(input.label, input.dates, None, Blacklist.randomUUID)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(blacklist.get)
    case _ => PartialSecureBlock(prime)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Blacklist]): Try[Set[Blacklist]] = {
    import controllers.crud.semester.BlacklistCRUDController._
    import models.semester.Semester.currentPredicate
    import defaultBindings.SemesterDescriptor

    queryString.foldLeft(Try(all)) {
      case (set, (`selectAttribute`, current)) if current.head == currentValue =>
        for {
          semesters <- repository.getAll[Semester].map(_.find(currentPredicate))
          blacklists <- set
        } yield semesters.fold(Set.empty[Blacklist]) { semester =>
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
      _ <- Future.fromTry(repository.add[Blacklist](blacklist))
    } yield Created(Json.toJson(blacklist)(writes)).as(mimeType)) recover {
      case NonFatal(t) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> t.getMessage
      ))
    }
  }
}
