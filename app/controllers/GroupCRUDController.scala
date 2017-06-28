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

    valueOf(countAttribute).fold[Option[Strategy]] {
      valueOf(minAttribute).zip(valueOf(maxAttribute)).map {
        case ((min, max)) => Range(min, max)
      }.headOption
    } { count =>
      Some(Count(count))
    }
  }
}

class GroupCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleServiceLike, val groupService: GroupServiceLike) extends AbstractCRUDController[GroupProtocol, Group, GroupAtom] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.groupV1Json

  override implicit val descriptor: Descriptor[Sesame, Group] = defaultBindings.GroupDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, GroupAtom] = defaultBindings.GroupAtomDescriptor

  override implicit val uriGenerator: UriGenerator[Group] = Group

  override implicit val reads: Reads[GroupProtocol] = Group.reads

  override implicit val writes: Writes[Group] = Group.writes

  override implicit val writesAtom: Writes[GroupAtom] = Group.writesAtom

  override protected def coAtomic(atom: GroupAtom): Group = Group(atom.label, atom.labwork.id, atom.members map (_.id), atom.invalidated, atom.id)

  override protected def compareModel(input: GroupProtocol, output: Group): Boolean = {
    input.label == output.label && input.members == output.members
  }

  override protected def fromInput(input: GroupProtocol, existing: Option[Group]): Group = existing match {
    case Some(group) => Group(input.label, input.labwork, input.members, group.invalidated, group.id)
    case None => Group(input.label, input.labwork, input.members)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, group.create)
    case GetAll => SecureBlock(restrictionId, group.getAll)
    case _ => PartialSecureBlock(god)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Group]): Try[Set[Group]] = {

    queryString.foldRight(Try[Set[Group]](all)) {
      case ((`labworkAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.labwork == p)))
      case ((`studentAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.members.contains(p))))
      case ((`labelAttribute`, v), t) => t.map(_.filter(_.label == v.head))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }

  def preview(course: String, labwork: String) = restrictedContext(course)(Create) action { implicit request =>
    import models.Group.protocolWrites

    optional2(strategyFrom(request.queryString))
      .flatMap(strategy => attempt(groupService.groupBy(UUID.fromString(labwork), strategy)))
      .map(_ map (g => GroupProtocol(g.label, g.labwork, g.members)))
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

  def atomic(groups: Set[Group]): Attempt[Set[GroupAtom]] = {
    import defaultBindings.{LabworkDescriptor, StudentDescriptor}

    SemanticUtils.collect {
      groups map { group =>
        for {
          optLabwork <- repository.get[SesameLabwork](SesameLabwork.generateUri(group.labwork))
          students <- repository.getMany[SesameStudent](group.members map User.generateUri)
        } yield optLabwork map (GroupAtom(group.label, _, students, group.invalidated, group.id))
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
