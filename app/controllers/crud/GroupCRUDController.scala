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

import scala.collection.Map
import scala.util.{Failure, Success}

class GroupCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService, val groupService: GroupServiceLike) extends AbstractCRUDController[GroupProtocol, Group] {
   override implicit def rdfWrites: ToPG[Sesame, Group] = defaultBindings.GroupBinding.groupBinder

   override implicit def rdfReads: FromPG[Sesame, Group] = defaultBindings.GroupBinding.groupBinder

   override implicit def classUrisFor: ClassUrisFor[Sesame, Group] = defaultBindings.GroupBinding.classUri

   override implicit def uriGenerator: UriGenerator[Group] = Group

   override implicit def reads: Reads[GroupProtocol] = Group.reads

   override implicit def writes: Writes[Group] = Group.writes

   override protected def fromInput(input: GroupProtocol, id: Option[UUID]): Group = id match {
      case Some(uuid) => Group(input.label, input.labwork, input.members, uuid)
      case None => Group(input.label, input.labwork, input.members, Group.randomUUID)
   }

   override def getWithFilter(queryString: Map[String, Seq[String]])(groups: Set[Group]): Result = ???

   override val mimeType: LwmMimeType = LwmMimeType.groupV1Json

   // POST /labworks/id/groups/size
   def createWithSize(labwork: String) = invokeAction(CreateRef)(Some(labwork)) secureContentTyped { implicit request =>
      request.body.validate[GroupSizeProtocol].fold(
         errors => {
            BadRequest(Json.obj(
               "status" -> "KO",
               "errors" -> JsError.toJson(errors)
            ))
         },
         success => {
            ???
         }
      )
   }

   // POST /labworks/id/groups/count
   def createWithCount(labwork: String) = invokeAction(CreateRef)(Some(labwork)) secureContentTyped { implicit request =>
      request.body.validate[GroupCountProtocol].fold(
         errors => {
            BadRequest(Json.obj(
               "status" -> "KO",
               "errors" -> JsError.toJson(errors)
            ))
         },
         success => {
            val labels = ('A' to 'Z').toVector.take(success.count)
            val participants = groupService.participantsFor(success.labwork).grouped(success.count).toList.zip(labels)
            val result = participants.map(p => Group(p._2.toString, success.labwork, p._1.toSet, Group.randomUUID)).map(g => repository.add(g))

            import utils.Ops._
            sequence(result) match {
               case Success(s) =>
                  Created(Json.toJson(s map rdfReads.fromPG)).as(mimeType)
               case Failure(e) =>
                  InternalServerError(Json.obj(
                     "status" -> "KO",
                     "errors" -> e.getMessage
                  ))
            }
         }
      )
   }

   def all(labwork: String) = invokeAction(AllRef)(Some(labwork)) securedAsync { request =>
      super.all()(request)
   }

   def update(labwork: String, id: String) = invokeAction(UpdateRef)(Some(labwork)) secureContentTypedAsync { request =>
      super.update(id)(request)
   }

   def get(labwork: String, id: String) = invokeAction(GetRef)(Some(labwork)) securedAsync { request =>
      super.get(id)(request)
   }

   def delete(labwork: String, id: String) = invokeAction(DeleteRef)(Some(labwork)) securedAsync { request =>
      super.delete(id)(request)
   }

   override protected def invokeAction(rule: Rule)(moduleId: Option[String]): Block = {
      import models.security.Permissions._

      Invoke {
         case CreateRef => Block(moduleId, Set(createGroup))
         case AllRef => Block(moduleId, Set(allGroups))
         case UpdateRef => Block(moduleId, Set(updateGroup))
         case GetRef => Block(moduleId, Set(getGroup))
         case DeleteRef => Block(moduleId, Set(deleteGroup))
         case _ => Block(None, Set.empty)
      }.run(rule)
   }
}
