package controllers.crud.labwork

import java.util.UUID

import controllers.crud.AbstractCRUDController
import controllers.crud.labwork.GroupCRUDController._
import models._
import models.labwork._
import models.security.Permissions._
import models.users.{Student, User}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.Request
import services.{GroupServiceLike, RoleService, SessionHandlingService}
import store.bind.Descriptor.Descriptor
import store.{Namespace, SemanticUtils, SesameRepository}
import utils.{Attempt, Continue, LwmMimeType, Return}
import utils.RequestOps._

import scala.collection.Map
import scala.util.{Failure, Success, Try}

object GroupCRUDController {
  val labworkAttribute = "labwork"
  val studentAttribute = "student"
  val labelAttribute = "label"
  val countAttribute = "value"
  val minAttribute = "min"
  val maxAttribute = "max"

  def range(min: Int, max: Int, s: Int): Int = ((min to max) reduce { (prev, curr) =>
    if (prev % s < curr % s) curr
    else prev
  }) + 1
}

class GroupCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService, val groupService: GroupServiceLike) extends AbstractCRUDController[GroupProtocol, Group, GroupAtom] {

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

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(group.get)
    case _ => PartialSecureBlock(god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, group.create)
    case Update => SecureBlock(restrictionId, group.update)
    case Delete => SecureBlock(restrictionId, group.delete)
    case Get => SecureBlock(restrictionId, group.get)
    case GetAll => SecureBlock(restrictionId, group.getAll)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Group]): Try[Set[Group]] = {
    import GroupCRUDController._

    queryString.foldRight(Try[Set[Group]](all)) {
      case ((`labworkAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.labwork == p)))
      case ((`studentAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.members.contains(p))))
      case ((`labelAttribute`, v), t) => t.map(_.filter(_.label == v.head))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }

  def createFrom(course: String, labwork: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    create(NonSecureBlock)(request)
  }

  def createAtomicFrom(course: String, labwork: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    createAtomic(NonSecureBlock)(request)
  }

  def updateFrom(course: String, labwork: String, group: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    update(group, NonSecureBlock)(rebase(Group.generateBase(UUID.fromString(group))))
  }

  def updateAtomicFrom(course: String, labwork: String, group: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    updateAtomic(group, NonSecureBlock)(rebase(Group.generateBase(UUID.fromString(group))))
  }

  def allFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    all(NonSecureBlock)(rebase(Group.generateBase, labworkAttribute -> Seq(labwork)))
  }

  def allAtomicFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    allAtomic(NonSecureBlock)(rebase(Group.generateBase, labworkAttribute -> Seq(labwork)))
  }

  def getFrom(course: String, labwork: String, group: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    get(group, NonSecureBlock)(rebase(Group.generateBase(UUID.fromString(group))))
  }

  def getAtomicFrom(course: String, labwork: String, group: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    getAtomic(group, NonSecureBlock)(rebase(Group.generateBase(UUID.fromString(group))))
  }

  def deleteFrom(course: String, labwork: String, group: String) = restrictedContext(course)(Delete) asyncAction { implicit request =>
    delete(group, NonSecureBlock)(rebase(Group.generateBase(UUID.fromString(group))))
  }

  def createWithRange(course: String, labwork: String) = restrictedContext(course)(Create) action { request =>
    groupBy(request, labwork)(applyRange)
      .flatMap(addLots)
      .mapResult(list => Created(Json.toJson(list.toSet)).as(mimeType))
  }

  def createWithCount(course: String, labwork: String) = restrictedContext(course)(Create) action { request =>
    groupBy(request, labwork)(applyCount)
      .flatMap(addLots)
      .mapResult(list => Created(Json.toJson(list.toSet)).as(mimeType))
  }

  def createAtomicWithRange(course: String, labwork: String) = restrictedContext(course)(Create) action { request =>
    groupBy(request, labwork)(applyRange)
      .flatMap(addLots)
      .flatMap(list => retrieveLots[GroupAtom](list map Group.generateUri))
      .mapResult(set => Created(Json.toJson(set)).as(mimeType))
  }

  def createAtomicWithCount(course: String, labwork: String) = restrictedContext(course)(Create) action { request =>
    groupBy(request, labwork)(applyCount)
      .flatMap(addLots)
      .flatMap(list => retrieveLots[GroupAtom](list map Group.generateUri))
      .mapResult(set => Created(Json.toJson(set)).as(mimeType))
  }

  def previewWithCount(course: String, labwork: String) = restrictedContext(course)(Create) action { request =>
    import Group._
    groupBy(request, labwork)(applyCount)
      .map(_ map (g => GroupProtocol(g.label, g.labwork, g.members)))
      .mapResult(set => Ok(Json.toJson(set)).as(mimeType))
  }

  def previewAtomicWithCount(course: String, labwork: String) = restrictedContext(course)(Create) action { request =>
    groupBy(request, labwork)(applyCount)
      .flatMap(atomic)
      .mapResult(set => Ok(Json.toJson(set)).as(mimeType))
  }

  def previewWithRange(course: String, labwork: String) = restrictedContext(course)(Create) action { request =>
    import Group._
    groupBy(request, labwork)(applyRange)
      .map(_ map (g => GroupProtocol(g.label, g.labwork, g.members)))
      .mapResult(set => Ok(Json.toJson(set)).as(mimeType))
  }

  def previewAtomicWithRange(course: String, labwork: String) = restrictedContext(course)(Create) action { request =>
    groupBy(request, labwork)(applyRange)
      .flatMap(atomic)
      .mapResult(set => Ok(Json.toJson(set)).as(mimeType))
  }

  private def applyRange(people: Vector[UUID], params: Map[String, Seq[String]]) = {
    for {
      min <- Try(params(minAttribute).head.toInt)
      max <- Try(params(maxAttribute).head.toInt) if min <= max
    } yield range(min, max, people.size)
  }

  private def applyCount(people: Vector[UUID], params: Map[String, Seq[String]]) = {
    Try(params(countAttribute).head.toInt) map (count => (people.size / count) + 1)
  }

  def atomic(groups: Set[Group]): Attempt[Set[GroupAtom]] = {
    import defaultBindings.{LabworkDescriptor, StudentDescriptor}

    SemanticUtils.collect {
      groups map { group =>
        for {
          optLabwork <- repository.get[Labwork](Labwork.generateUri(group.labwork))
          students <- repository.getMany[Student](group.members map User.generateUri)
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

  def groupBy[A](request: Request[A], labwork: String)(f: (Vector[UUID], Map[String, Seq[String]]) => Try[Int]): Attempt[Set[Group]] = {
    (for {
      people <- groupService sortApplicantsFor UUID.fromString(labwork) if people.nonEmpty
      groupSize <- f(people, request.queryString)
      grouped = people.grouped(groupSize).toList
      zipped = groupService.alphabeticalOrdering(grouped.size) zip grouped
      mapped = zipped map (t => Group(t._1, UUID.fromString(labwork), t._2.toSet))
    } yield mapped) match {
      case Success(a) => Continue(a.toSet)
      case Failure(e) => Return(
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> s"Error while creating groups for labwork: ${e.getMessage}"
        )))
    }
  }
}
