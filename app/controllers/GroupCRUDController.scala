package controllers

import java.util.UUID

import models._
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import services._
import store.bind.Descriptor.Descriptor
import store.{Namespace, SemanticUtils, SesameRepository}
import utils.{Attempt, Continue, LwmMimeType, Return}
import controllers.GroupCRUDController._
import scala.util.{Failure, Success, Try}
import models.Permissions.{group, god}

object GroupCRUDController {
  val labworkAttribute = "labwork"
  val studentAttribute = "student"
  val labelAttribute = "label"

  val countAttribute = "count"
  val minAttribute = "min"
  val maxAttribute = "max"

  def strategyFrom(queryString: Map[String, Seq[String]]) = {
    def valueOf(attribute: String) = queryString.get(attribute).flatMap(_.headOption)

    valueOf(countAttribute).fold[Option[GroupingStrategy]] {
      valueOf(minAttribute).zip(valueOf(maxAttribute)).map {
        case ((min, max)) => RangeGrouping(min, max)
      }.headOption
    } { count =>
      Some(CountGrouping(count))
    }
  }
}

class GroupCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleServiceLike, val groupService: GroupServiceLike) extends AbstractCRUDController[SesameGroupProtocol, SesameGroup, SesameGroupAtom] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.groupV1Json

  override implicit val descriptor: Descriptor[Sesame, SesameGroup] = defaultBindings.GroupDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, SesameGroupAtom] = defaultBindings.GroupAtomDescriptor

  override implicit val uriGenerator: UriGenerator[SesameGroup] = SesameGroup

  override implicit val reads: Reads[SesameGroupProtocol] = SesameGroup.reads

  override implicit val writes: Writes[SesameGroup] = SesameGroup.writes

  override implicit val writesAtom: Writes[SesameGroupAtom] = SesameGroup.writesAtom

  override protected def coAtomic(atom: SesameGroupAtom): SesameGroup = SesameGroup(atom.label, atom.labwork.id, atom.members map (_.id), atom.invalidated, atom.id)

  override protected def compareModel(input: SesameGroupProtocol, output: SesameGroup): Boolean = {
    input.label == output.label && input.members == output.members
  }

  override protected def fromInput(input: SesameGroupProtocol, existing: Option[SesameGroup]): SesameGroup = existing match {
    case Some(group) => SesameGroup(input.label, input.labwork, input.members, group.invalidated, group.id)
    case None => SesameGroup(input.label, input.labwork, input.members)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, group.create)
    case GetAll => SecureBlock(restrictionId, group.getAll)
    case _ => PartialSecureBlock(god)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[SesameGroup]): Try[Set[SesameGroup]] = {

    queryString.foldRight(Try[Set[SesameGroup]](all)) {
      case ((`labworkAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.labwork == p)))
      case ((`studentAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.members.contains(p))))
      case ((`labelAttribute`, v), t) => t.map(_.filter(_.label == v.head))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }

  def preview(course: String, labwork: String) = restrictedContext(course)(Create) action { implicit request =>
    import models.SesameGroup.protocolWrites

    optional2(strategyFrom(request.queryString))
      .flatMap(strategy => attempt(groupService.groupBy(UUID.fromString(labwork), strategy)))
      .map(_ map (g => SesameGroupProtocol(g.label, g.labwork, g.members)))
      .mapResult(set => Ok(Json.toJson(set)).as(mimeType))
  }

  def previewAtomic(course: String, labwork: String) = restrictedContext(course)(Create) action { implicit request =>
    optional2(strategyFrom(request.queryString))
      .flatMap(strategy => attempt(groupService.groupBy(UUID.fromString(labwork), strategy)))
      .flatMap(atomic)
      .mapResult(set => Ok(Json.toJson(set)).as(mimeType))
  }

  def allFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    all(NonSecureBlock)(rebase(labworkAttribute -> Seq(labwork)))
  }

  def allAtomicFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    allAtomic(NonSecureBlock)(rebase(labworkAttribute -> Seq(labwork)))
  }

  def atomic(groups: Set[SesameGroup]): Attempt[Set[SesameGroupAtom]] = {
    import defaultBindings.{LabworkDescriptor, StudentDescriptor}

    SemanticUtils.collect {
      groups map { group =>
        for {
          optLabwork <- repository.get[SesameLabwork](SesameLabwork.generateUri(group.labwork))
          students <- repository.getMany[SesameStudent](group.members map User.generateUri)
        } yield optLabwork map (SesameGroupAtom(group.label, _, students, group.invalidated, group.id))
      }
    } match {
      case Success(set) => Continue(set)
      case Failure(e) => Return(InternalServerError(
        Json.obj(
          "status" -> "KO",
          "errors" -> s"Error while generating groups for labwork: ${e.getMessage}"
        )))
    }
  }
}
