package controllers.crud

import java.util.UUID

import models._
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.Result
import services.{GroupServiceLike, RoleService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import security.Permissions._
import scala.collection.Map
import scala.util.{Success, Failure, Try}

object GroupCRUDController {

  val labworkAttribute = "labwork"
}

class GroupCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService, val groupService: GroupServiceLike) extends AbstractCRUDController[GroupProtocol, Group] {
  override val mimeType: LwmMimeType = LwmMimeType.groupV1Json

  override implicit def rdfWrites: ToPG[Sesame, Group] = defaultBindings.GroupBinding.groupBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Group] = defaultBindings.GroupBinding.classUri

  override implicit def uriGenerator: UriGenerator[Group] = Group

  override implicit def reads: Reads[GroupProtocol] = Group.reads

  override implicit def writes: Writes[Group] = Group.writes

  override def getWithFilter(queryString: Map[String, Seq[String]])(groups: Set[Group]): Result = {
    import GroupCRUDController._

    val filtered = queryString.foldRight(Try[Set[Group]](groups)) {
      case ((`labworkAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.labwork == p)))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
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

      case Failure(e) => BadRequest(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }

  //TODO: Repair information inconsistency
  // POST /labworks/id/groups/range
  def createWithRange(labwork: String) = restrictedContext(labwork)(Create) contentTypedAction { implicit request =>
    request.body.validate[GroupRangeProtocol].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => {
        def size(min: Int, max: Int, s: Int): Int = ((min to max) reduce { (prev, curr) =>
          if (prev % s < curr % s) curr
          else prev
        }) + 1

        val processed =
          for {
            people <- groupService.sortApplicantsFor(success.labwork)
            groupSize = size(success.min, success.max, people.size)
            grouped = people.grouped(groupSize).toList
            zipped = groupService.alphabeticalOrdering(grouped.size) zip grouped
            mapped = zipped map (t => Group(t._1, success.labwork, t._2.toSet))
            _ <- repository.addMany[Group](mapped).toOption
          } yield mapped

        processed match {
          case Some(groups) => Ok(Json.toJson(groups)).as(mimeType)
          case None => InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> s"Error while creating groups for labwork ${success.labwork}"
          ))
        }
      }
    )
  }

  // POST /labworks/id/groups/count
  def createWithCount(labwork: String) = restrictedContext(labwork)(Create) contentTypedAction { implicit request =>
    request.body.validate[GroupCountProtocol].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => {
        val processed =
          for {
            people <- groupService.sortApplicantsFor(success.labwork)
            groupSize = (people.size / success.count) + 1
            grouped = people.grouped(groupSize).toList
            zipped = groupService.alphabeticalOrdering(grouped.size) zip grouped
            mapped = zipped map (t => Group(t._1, success.labwork, t._2.toSet))
            _ <- repository.addMany[Group](mapped).toOption
          } yield mapped

        processed match {
          case Some(groups) => Ok(Json.toJson(groups)).as(mimeType)
          case None => InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> s"Error while creating groups for labwork ${success.labwork}"
          ))
        }
      }
    )
  }

  override implicit def rdfReads: FromPG[Sesame, Group] = defaultBindings.GroupBinding.groupBinder

  def allFrom(labwork: String) = restrictedContext(labwork)(All) asyncAction { request =>
    super.all(NonSecureBlock)(request)
  }

  def updateFrom(labwork: String, id: String) = restrictedContext(labwork)(Update) asyncContentTypedAction { request =>
    super.update(id, NonSecureBlock)(request)
  }

  def getFrom(labwork: String, id: String) = restrictedContext(labwork)(Get) asyncAction { request =>
    super.get(id, NonSecureBlock)(request)
  }

  def deleteFrom(labwork: String, id: String) = restrictedContext(labwork)(Delete) asyncAction { request =>
    super.delete(id, NonSecureBlock)(request)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Set(prime))
  }

  override protected def restrictedContext(moduleId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(moduleId, Set(createGroup))
    case All => SecureBlock(moduleId, Set(allGroups))
    case Update => SecureBlock(moduleId, Set(updateGroup))
    case Get => SecureBlock(moduleId, Set(getGroup))
    case Delete => SecureBlock(moduleId, Set(deleteGroup))
    case _ => NonSecureBlock
  }

  override protected def fromInput(input: GroupProtocol, id: Option[UUID]): Group = id match {
    case Some(uuid) => Group(input.label, input.labwork, input.members, uuid)
    case None => Group(input.label, input.labwork, input.members, Group.randomUUID)
  }

}
