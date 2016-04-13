package controllers.crud.labwork

import java.util.UUID

import controllers.crud.AbstractCRUDController
import controllers.crud.labwork.GroupCRUDController._
import models._
import models.labwork._
import models.security.Permissions._
import models.users.{Student, User}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.Result
import services.{GroupServiceLike, RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
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

class GroupCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService, val groupService: GroupServiceLike) extends AbstractCRUDController[GroupProtocol, Group] {

  override val mimeType: LwmMimeType = LwmMimeType.groupV1Json

  override implicit def rdfWrites: ToPG[Sesame, Group] = defaultBindings.GroupBinding.groupBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Group] = defaultBindings.GroupBinding.classUri

  override implicit def uriGenerator: UriGenerator[Group] = Group

  override implicit def reads: Reads[GroupProtocol] = Group.reads

  override implicit def writes: Writes[Group] = Group.writes

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

  def createWithRange(course: String, labwork: String) = groupBy(course, labwork) { (people, params) =>
    applyRange(people, params)
  } { groups =>
    createGroups(groups)
  }

  def createWithCount(course: String, labwork: String) = groupBy(course, labwork) { (people, params) =>
    applyCount(people, params)
  } { groups =>
    createGroups(groups)
  }

  def createAtomicWithRange(course: String, labwork: String) = groupBy(course, labwork) { (people, params) =>
    applyRange(people, params)
  } { groups =>
    createAtomic(groups)
  }

  def createAtomicWithCount(course: String, labwork: String) = groupBy(course, labwork) { (people, params) =>
    applyCount(people, params)
  } { groups =>
    createAtomic(groups)
  }

  def previewWithCount(course: String, labwork: String) = groupBy(course, labwork) { (people, params) =>
    applyCount(people, params)
  } { groups =>
    returnGroups(groups)
  }

  def previewAtomicWithCount(course: String, labwork: String) = groupBy(course, labwork) { (people, params) =>
    applyCount(people, params)
  } { groups =>
    returnAtomic(groups)
  }

  def previewWithRange(course: String, labwork: String) = groupBy(course, labwork) { (people, params) =>
    applyRange(people, params)
  } { groups =>
    returnGroups(groups)
  }

  def previewAtomicWithRange(course: String, labwork: String) = groupBy(course, labwork) { (people, params) =>
    applyRange(people, params)
  } { groups =>
    returnAtomic(groups)
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

  private def returnGroups(groups: List[Group]) = Success(Ok(Json.toJson(groups)).as(mimeType))

  private def returnAtomic(groups: List[Group]) = atomizeMany(groups.toSet).map(Ok(_).as(mimeType))

  private def createGroups(groups: List[Group]) = repository.addMany(groups).map(_ => Created(Json.toJson(groups)).as(mimeType))

  private def createAtomic(groups: List[Group]) = repository.addMany(groups).flatMap(_ => atomizeMany(groups.toSet)).map(Created(_).as(mimeType))

  private def groupBy(course: String, labwork: String)
                      (grouping: (Vector[UUID], Map[String, Seq[String]]) => Try[Int])
                      (serialise: List[Group] => Try[Result]) = restrictedContext(course)(Create) action { request =>
    (for {
      people <- groupService.sortApplicantsFor(UUID.fromString(labwork)) if people.nonEmpty
      groupSize <- grouping(people, request.queryString)
      grouped = people.grouped(groupSize).toList
      zipped = groupService.alphabeticalOrdering(grouped.size) zip grouped
      mapped = zipped map (t => Group(t._1, UUID.fromString(labwork), t._2.toSet))
      res <- serialise(mapped)
    } yield res) match {
      case Success(result) => result
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> s"Error while creating groups for labwork: ${e.getMessage}"
        ))
    }
  }

  override implicit def rdfReads: FromPG[Sesame, Group] = defaultBindings.GroupBinding.groupBinder

  override protected def fromInput(input: GroupProtocol, existing: Option[Group]): Group = existing match {
    case Some(group) => Group(input.label, input.labwork, input.members, group.id)
    case None => Group(input.label, input.labwork, input.members, Group.randomUUID)
  }

  override protected def compareModel(input: GroupProtocol, output: Group): Boolean = {
    input.label == output.label && input.members == output.members
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

  override protected def atomize(output: Group): Try[Option[JsValue]] = {
    import Group.atomicWrites
    import defaultBindings.LabworkBinding._
    import defaultBindings.StudentBinding._

    for {
      labwork <- repository.get[Labwork](Labwork.generateUri(output.labwork)(namespace))
      students <- repository.getMany[Student](output.members.map(id => User.generateUri(id)(namespace)))
    } yield labwork.map { l =>
      Json.toJson(GroupAtom(output.label, l, students, output.id))
    }
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
}
